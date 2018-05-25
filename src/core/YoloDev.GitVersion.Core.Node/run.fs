[<AutoOpen>]
module YoloDev.GitVersion.NodeRun

open Bindings.NodeGit
open System
open YoloDev.GitVersion.Core.Node.Types
open YoloDev.GitVersion.Core.Node.Shim
open YoloDev.GitVersion.Core.Abstractions
open Fable.Core
open Fable.PowerPack

module private Env =
  [<Emit("process.env[$0] || null")>]
  let get (_: string) : string = jsNative

type SystemImpl () =

  interface ISystem with

    member __.GetEnv n = IO.delay <| fun () ->
      match Env.get n with
      | null -> IO.unit None
      | value -> IO.unit (Some value)
    
    member __.OpenRepository p =
      IO.ofPromiseFactory (fun () -> nodegit.Repository.``open`` p)
      |> IO.map (fun r -> new Repository (r))
      |> IO.map (fun r -> new RepositoryWrapper (r) :> IRepository)

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