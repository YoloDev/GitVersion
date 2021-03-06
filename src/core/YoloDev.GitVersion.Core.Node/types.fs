module internal YoloDev.GitVersion.Core.Node.Types

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Node
open Fable.PowerPack
open Bindings.Winston
open System
open YoloDev.GitVersion.Core.Node.Shim
open YoloDev.GitVersion.Core.Abstractions
open YoloDev.GitVersion.Core.Logging
open YoloDev.GitVersion.SystemBuilders

let logger = Logger.create "YoloDev.GitVersion.Core.Node.Types"

[<AutoOpen>]
module private Helpers =

  let dispose (d: #IDisposable) = d.Dispose ()

type CommitWrapper (commit: Commit) =
  
  interface ICommit with
    member __.Sha = IO.unit commit.Sha
    member __.Message = commit.Message
    
  interface IDisposable with
    member __.Dispose () = dispose commit

[<RequireQualifiedAccess>]
module CommitWrapper =

  let ofCommit c = new CommitWrapper (c) :> ICommit

type TagWrapper (tag: Tag) =
  
  interface ITag with
    member __.Name = IO.unit tag.FriendlyName
    member __.Sha = tag.Target |> IO.map (fun t -> t.Sha)
    member __.Commit = 
      io {
        let! target = tag.Target
        let! commit = target.Peel<Commit> typeof<Commit>

        return new CommitWrapper (Option.get commit) :> ICommit
      }
  
  interface IDisposable with
    member __.Dispose () = ()

[<RequireQualifiedAccess>]
module TagWrapper =

  let ofTag tag = new TagWrapper (tag) :> ITag

type BranchWrapper (branch: Branch) =

  member internal __.Branch = branch

  interface IBranch with
    member __.Name = IO.unit branch.FriendlyName
    member __.Tip = 
      branch.Tip
      |> IO.map (Option.map CommitWrapper.ofCommit)
  
  interface IDisposable with
    member __.Dispose () = ()

[<RequireQualifiedAccess>]
module BranchWrapper =

  let ofBranch branch = new BranchWrapper (branch) :> IBranch

type CommitLogWrapper (repo: Repository, commitLog: QueryableCommitLog) =

  interface ICommitLog with

    member __.Get s =
      repo.Lookup<Commit> (s, typeof<Commit>)
      |> IO.map (Option.map (fun c -> new CommitWrapper (c) :> ICommit))

    member __.Seq =
      commitLog.ToSeq ()
      |> IOSeq.map (fun c -> new CommitWrapper (c) :> ICommit)
    
    member __.Query f =
      (commitLog.QueryBy f).ToSeq ()
      |> IOSeq.map (fun c -> new CommitWrapper (c) :> ICommit)
  
  interface IDisposable with
    member __.Dispose () = ()

type RepositoryWrapper (repo: Repository, dispose: (unit -> unit) option) =

  new (repo: Repository) = new RepositoryWrapper (repo, None)
  new (repo: Repository, dispose: (unit -> unit)) = new RepositoryWrapper (repo, Some dispose)

  interface IRepository with
    member __.Tags =
      repo.Tags.Seq
      |> IOSeq.map (fun t -> new TagWrapper (t) :> ITag)
    
    member __.Commits =
      new CommitLogWrapper (repo, repo.Commits) :> ICommitLog
    
    member __.Head = 
      repo.Head
      |> IO.map (fun head -> new BranchWrapper (head) :> IBranch)
    
    member __.Commit (message: string) =
      io {
        do! Logger.debug logger (
              eventX "Commit {message}"
              >> setField "message" message)
        let author = { Signature.name = "Test"; email = "test@yolodev" }
          
        let! commit = repo.Commit (message, author, author)
        return CommitWrapper.ofCommit commit
      }
    
    member __.Tag (name: string) =
      io {
        do! Logger.debug logger (
              eventX "Tag {name}"
              >> setField "name" name)

        let! tag = repo.ApplyTag name
        return TagWrapper.ofTag tag
      }
    
    member __.IsDirty =
      io {
        let! status = repo.RetrieveStatus None

        return! status.IsDirty
      }
    
    member __.CreateBranch name =
      io {
        debugger ()
        let! branch = repo.CreateBranch name

        debugger ()
        return BranchWrapper.ofBranch (Option.get branch)
      }
    
    member __.Checkout branch =
      let branch =
        match branch with
        | :? BranchWrapper as b -> b.Branch
        | _ -> failwithf "Invalid branch type used"
      Commands.Checkout (repo, branch)
      |> IO.map ignore

  interface IDisposable with
    member __.Dispose () = 
      (repo :> System.IDisposable).Dispose ()
      match dispose with
      | Some d -> d ()
      | _      -> ()

type WinstonLogMessage = 
  { logger: string
    level: string
    message: string
    fields: Map<string, obj>
    exceptions: exn list }

[<Pojo>]
type IComonLogMessage =
  inherit Bindings.Winston.ILogMessage
  abstract scope: string with get, set
  abstract fields: obj with get, set
  abstract exceptions: exn array with get, set

module WinstonLogMessage =

  let forMsg logger level message =
    { logger = logger
      level = level
      message = message
      fields = Map.empty
      exceptions = List.empty }

  let setField n v m =
    { m with fields = Map.add n v m.fields }
  
  let addExn e m =
    { m with exceptions = e :: m.exceptions }
  
  let toJs msg =
    let js = createEmpty<IComonLogMessage>
    js.level <- msg.level
    js.message <- msg.message
    js.scope <- msg.logger
    
    if not (Map.isEmpty msg.fields)
    then 
      js.fields <-
        msg.fields
        |> Map.toSeq
        |> Seq.map (fun (k, v) -> k ==> v)
        |> createObj
    
    if not (List.isEmpty msg.exceptions)
    then
      js.exceptions <-
        msg.exceptions
        |> Seq.rev
        |> Array.ofSeq
    
    js


type LogMessageWrapper (msg: WinstonLogMessage) =

  member __.Message = msg

  interface ILogMessage with

    member __.SetField n v =
      LogMessageWrapper (WinstonLogMessage.setField n v msg)
      :> ILogMessage

    member __.AddExn e =
      LogMessageWrapper (WinstonLogMessage.addExn e msg)
      :> ILogMessage

type LoggerWrapper (logger: Bindings.Winston.Logger, name: string) =

  let factory level =
    { new ILogMessageFactory with
      member __.Event s = 
        WinstonLogMessage.forMsg name level s
        |> LogMessageWrapper
        :> ILogMessage }
  
  let logMessage level (f: ILogMessageFactory -> ILogMessage) = IO.delay <| fun () ->
    let msg =
      match f (factory level) with
      | :? LogMessageWrapper as wrapper ->
        WinstonLogMessage.toJs wrapper.Message
      | _ -> failwithf "Invalid message factory provided"
    
    logger.log msg
    IO.zero
  
  interface ILogger with
    member __.LogVerbose f = logMessage "verbose" f
    member __.LogDebug f = logMessage "debug" f
    member __.LogInfo f = logMessage "info" f
    member __.LogWarn f = logMessage "warn" f
    member __.LogError f = logMessage "error" f
    member __.LogFatal f = logMessage "fatal" f

type FsEntryWrapper (stat: Fs.Stats, inPath: string) =
  let p = path.normalize inPath

  let entries =
    let paths =
      IOSeq.ofSeqPromiseFactory <| fun () ->
        Promise.create <| fun res rej ->
          fs.readdir (U2.Case1 p, fun err entries ->
            match err with
            | None -> res (entries |> Seq.map (fun e -> path.resolve [| p; e |]))
            | Some e -> rej (e :?> exn))
    
    paths
    |> IOSeq.mapM FsEntryWrapper.Lookup

  static member Lookup p =
    let stat =
      IO.ofPromiseFactory <| fun () ->
        Promise.create <| fun res rej ->
          fs.stat (U2.Case1 p, fun err stat ->
            match err with
            | None -> res stat
            | Some e -> rej (e :?> exn)
            Globals.undefined)
    
    stat
    |> IO.map (fun stat -> FsEntryWrapper (stat, p))

  interface IFileSystemEntry with
    member __.Path = p
    member __.Name = path.basename p
    member __.IsDirectory = stat.isDirectory ()
    member __.IsFile = stat.isFile ()
  
  interface IFile with
    member __.Read () =
      IO.ofPromiseFactory <| fun () ->
        Promise.create <| fun res rej ->
          fs.readFile (p, "utf8", fun err content ->
            match err with
            | None -> res content
            | Some e -> rej (e :?> exn))

  interface IDirectory with
    member __.Entries =
      entries
      |> IOSeq.map (fun e -> e :> IFileSystemEntry)
    
    member __.Dirs =
      entries
      |> IOSeq.filter (fun e -> (e :> IFileSystemEntry).IsDirectory)
      |> IOSeq.map (fun e -> e :> IDirectory)
    
    member __.Files =
      entries
      |> IOSeq.filter (fun e -> (e :> IFileSystemEntry).IsFile)
      |> IOSeq.map (fun e -> e :> IFile)
