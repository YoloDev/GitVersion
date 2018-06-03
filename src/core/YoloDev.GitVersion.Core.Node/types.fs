module internal YoloDev.GitVersion.Core.Node.Types

open System
open YoloDev.GitVersion.Core.Node.Shim
open YoloDev.GitVersion.Core.Abstractions
open YoloDev.GitVersion.Core.Logging
open YoloDev.GitVersion.Core.Logging.Message
open YoloDev.GitVersion.SystemBuilders

let logger = Log.create "YoloDev.GitVersion.Core.Node.Types"

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

  interface IBranch with
    member __.Name = IO.unit branch.FriendlyName
    member __.Tip = 
      branch.Tip
      |> IO.map (Option.map CommitWrapper.ofCommit)
  
  interface IDisposable with
    member __.Dispose () = ()

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

  interface IDisposable with
    member __.Dispose () = 
      (repo :> System.IDisposable).Dispose ()
      match dispose with
      | Some d -> d ()
      | _      -> ()
