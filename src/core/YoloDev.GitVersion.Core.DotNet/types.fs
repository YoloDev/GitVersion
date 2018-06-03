module internal YoloDev.GitVersion.Core.DotNet.Types

open System
open LibGit2Sharp
open YoloDev.GitVersion.Core.Abstractions
open YoloDev.GitVersion.SystemBuilders
open YoloDev.GitVersion.Core.Logging
open YoloDev.GitVersion.Logary.Facade

let logger = Logger.create "YoloDev.GitVersion.Core.DotNet.Types"

[<AutoOpen>]
module internal Helpers =

  [<RequireQualifiedAccess>]
  module IO =

    let waitForFS =
      Async.Sleep 3000 |> IO.ofAsync
    
    let ofJob job = IO <| fun _ cont ->
      let ok = Result.Ok >> cont >> ignore
      let error = Result.Error >> cont >> ignore
      let cancelled = Result.Error >> cont >> ignore
      Async.StartWithContinuations (job, ok, error, cancelled)
      // startWithActions (Result.Error >> cont >> ignore) (Result.Ok >> cont >> ignore) job
      FakeUnit

type CommitWrapper (commit: Commit) =
  
  interface ICommit with
    member __.Sha = IO.unit commit.Sha
    member __.Message = IO.unit commit.Message
    
  interface IDisposable with
    member __.Dispose () = ()

[<RequireQualifiedAccess>]
module CommitWrapper =

  let ofCommit c = new CommitWrapper (c) :> ICommit

type TagWrapper (tag: Tag) =
  
  interface ITag with
    member __.Name = IO.unit tag.FriendlyName
    member __.Sha = IO.unit tag.Target.Sha
    member __.Commit = 
      IO.delay <| 
        fun () -> IO.unit (new CommitWrapper (tag.Target.Peel<Commit> ()) :> ICommit)
  
  interface IDisposable with
    member __.Dispose () = ()

[<RequireQualifiedAccess>]
module TagWrapper =

  let ofTag tag = new TagWrapper (tag) :> ITag

type BranchWrapper (branch: Branch) =

  interface IBranch with
    member __.Name = IO.unit branch.FriendlyName
    member __.Tip = 
      branch.Tip
      |> Option.ofObj
      |> Option.map CommitWrapper.ofCommit
      |> IO.unit
  
  interface IDisposable with
    member __.Dispose () = ()

type CommitLogWrapper (repo: Repository, commitLog: IQueryableCommitLog) =

  interface ICommitLog with

    member __.Get s =
      repo.Lookup<Commit> s
      |> Option.ofObj
      |> Option.map (fun c -> new CommitWrapper (c) :> ICommit)
      |> IO.unit

    member __.Seq =
      commitLog
      |> Seq.map (fun c -> new CommitWrapper (c) :> ICommit)
      |> IOSeq.ofSeq
    
    member __.Query f =
      let filter = 
        LibGit2Sharp.CommitFilter (FirstParentOnly = f.FirstParentOnly,
          IncludeReachableFrom = Array.ofList f.IncludeReachableFrom,
          ExcludeReachableFrom = Array.ofList f.ExcludeReachableFrom)
      
      commitLog.QueryBy filter
      |> Seq.map (fun c -> new CommitWrapper (c) :> ICommit)
      |> IOSeq.ofSeq
  
  interface IDisposable with
    member __.Dispose () = ()

type RepositoryWrapper (repo: Repository, dispose: (unit -> unit) option) =

  new (repo: Repository) = new RepositoryWrapper (repo, None)
  new (repo: Repository, dispose: (unit -> unit)) = new RepositoryWrapper (repo, Some dispose)

  interface IRepository with
    member __.Tags =
      repo.Tags
      |> Seq.map (fun t -> new TagWrapper (t) :> ITag)
      |> IOSeq.ofSeq
    
    member __.Commits =
      new CommitLogWrapper (repo, repo.Commits) :> ICommitLog
    
    member __.Head = 
      IO.delay <|
        fun () -> IO.unit (new BranchWrapper (repo.Head) :> IBranch)
    
    member __.Commit (message: string) =
      io {
        do! Logger.debug logger (
              eventX "Commit {message}"
              >> setField "message" message)
        
        let author = Signature ("Test", "@yolodev", DateTimeOffset.Now)
          
        let commit = repo.Commit (message, author, author, CommitOptions (AllowEmptyCommit = true)) |> CommitWrapper.ofCommit
        do! IO.waitForFS
        return commit
      }
    
    member __.Tag (name: string) =
      io {
        do! Logger.debug logger (
              eventX "Tag {name}"
              >> setField "name" name)
        
        let tag = repo.ApplyTag name |> TagWrapper.ofTag
        do! IO.waitForFS
        return tag
      }
    
    member __.IsDirty =
      IO.delay <|
        fun () ->
          let status = repo.RetrieveStatus ()
          IO.unit status.IsDirty

  interface IDisposable with
    member __.Dispose () = 
      repo.Dispose ()
      match dispose with
      | Some d -> d ()
      | _      -> ()

type LogMessageWrapper (msg: LogLevel -> Message) =

  member __.Message = msg

  interface ILogMessage with
    member __.SetField n v =
      LogMessageWrapper (msg >> Message.setField n v)
      :> ILogMessage
    
    member __.AddExn e =
      LogMessageWrapper (msg >> Message.addExn e)
      :> ILogMessage

type LoggerWrapper (logger: Logger) =

  let factory =
    { new ILogMessageFactory with
      member __.Event s =
        LogMessageWrapper (Message.eventX s)
        :> ILogMessage }
  
  let createMessage (f: ILogMessageFactory -> ILogMessage) =
    match f factory with
    | :? LogMessageWrapper as wrapper ->
      wrapper.Message
    | _ -> failwithf "Invalid message factory provided"
  
  interface ILogger with
    member __.LogVerbose f = 
      logger.verboseWithBP (createMessage f)
      |> IO.ofJob
    
    member __.LogDebug f =
      logger.debugWithBP (createMessage f)
      |> IO.ofJob
    
    member __.LogInfo f =
      logger.infoWithBP (createMessage f)
      |> IO.ofJob
    
    member __.LogWarn f =
      logger.warnWithBP (createMessage f)
      |> IO.ofJob
    

    member __.LogError f =
      logger.warnWithBP (createMessage f)
      |> IO.ofJob
    
    member __.LogFatal f =
      logger.fatalWithBP (createMessage f)
      |> IO.ofJob
      