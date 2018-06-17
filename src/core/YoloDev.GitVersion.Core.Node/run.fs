[<AutoOpen>]
module YoloDev.GitVersion.NodeRun

open Bindings.NodeGit
open Bindings.Winston
open Bindings.Chalk
open System
open YoloDev.GitVersion.Core.Node.Types
open YoloDev.GitVersion.Core.Node.Shim
open YoloDev.GitVersion.Core.Abstractions
open Fable.Core
open Fable.Core.JsInterop
open Fable.PowerPack

module private Helpers =
  [<Emit("String($0.stack || $0)")>]
  let errorStr (_: exn) : string = jsNative

  [<Emit("$0[$1]")>]
  let getOpt (_: obj) (_: string): obj option = jsNative

module private Env =
  [<Emit("process.env[$0] || null")>]
  let get (_: string) : string = jsNative

type SystemImpl () =

  let logger = 
    let regex = System.Text.RegularExpressions.Regex ("\\{(\\w+)\\}")
    let opts = createEmpty<LoggerFactoryArgs>
    let levels = 
      createObj [
        "fatal" ==> 0
        "error" ==> 1
        "warn" ==> 2
        "info" ==> 3
        "debug" ==> 4
        "verbose" ==> 5
      ]
    
    let shortCode =
      function
      | "fatal"   -> "FAT"
      | "error"   -> "ERR"
      | "warn"    -> "WRN"
      | "info"    -> "INF"
      | "debug"   -> "DBG"
      | "verbose" -> "VRB"
      | n         -> (n.ToUpperInvariant ()).Substring (0, 3)
    
    opts.levels <- levels
    opts.level <- "verbose"
    opts.transports <- [| winston.transports.Console () |]
    opts.format <- 
      winston.format.combine [
        winston.format.timestamp ()
        winston.format.printf <| fun m ->
          let msg = m :?> IComonLogMessage
          let text = regex.Replace (msg.message, fun m ->
            let name = m.Groups.[1].Value
            match Helpers.getOpt msg.fields name with
            | None   -> sprintf "{%s}" (chalk.bgRed.white.bold.color name)
            | Some v -> chalk.blue.color (sprintf "%O" v))
          
          //let text = sprintf "%s [%s] %s: %s" (string (Helpers.getOpt msg "timestamp")) msg.scope msg.level text
          let text = sprintf "%s [%s]: %s" (shortCode msg.level) (msg.scope.Replace ("YoloDev.GitVersion.", "")) text
          let text =
            match msg.level with
            | "verbose" -> chalk.dim.magenta.color text
            | "debug"   -> chalk.dim.color text
            | "warn"    -> chalk.yellow.color text
            | "error"   -> chalk.red.color text
            | "fatal"   -> chalk.red.bold.color text
            | _         -> text

          match unbox msg.exceptions with
          | None -> text
          | Some (xs: exn array) ->
            let errors = String.concat "\n\n" (Array.map Helpers.errorStr xs)
            sprintf "%s\n\n%s" text errors
      ]

    winston.createLogger opts

  interface ISystem with

    member __.GetEnv n = IO.delay <| fun () ->
      match Env.get n with
      | null -> IO.unit None
      | value -> IO.unit (Some value)
    
    member __.OpenRepository p =
      IO.ofPromiseFactory (fun () -> nodegit.Repository.``open`` p)
      |> IO.map (fun r -> new Repository (r))
      |> IO.map (fun r -> new RepositoryWrapper (r) :> IRepository)
    
    member __.GetLogger name =
      LoggerWrapper (logger, name)
      :> ILogger
      |> IO.unit
    
    member __.GetDir path =
      FsEntryWrapper.Lookup path
      |> IO.map (fun e -> 
        if (e :> IFileSystemEntry).IsDirectory
        then e :> IDirectory
        else failwithf "Path %s is not a directory" path)

[<RequireQualifiedAccess>]
module IO =

  let run (ma: #IO<_>) =
    Promise.create <|
      fun cok cerr ->
        let sys = SystemImpl ()
        IO.fork ma sys <|
          function
          | Ok a -> 
            cok a
            FakeUnit
          | Error e ->
            cerr e
            FakeUnit
        |> ignore
