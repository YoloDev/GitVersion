module internal YoloDev.GitVersion.Core.Node.Types

open System
open YoloDev.GitVersion.Core.Node.Shim
open YoloDev.GitVersion.Core.Abstractions
open YoloDev.GitVersion.SystemBuilders

[<AutoOpen>]
module private Helpers =

  let inline dispose (d: #IDisposable) = d.Dispose ()

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
      tag.Target
      |> IO.bind (fun t -> t.Peel<Commit> typeof<Commit>)
      |> IO.map (fun c -> new CommitWrapper (c) :> ICommit)
  
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

type CommitLogWrapper (commitLog: QueryableCommitLog) =

  interface ICommitLog with
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
      new CommitLogWrapper (repo.Commits) :> ICommitLog
    
    member __.Head = 
      repo.Head
      |> IO.map (fun head -> new BranchWrapper (head) :> IBranch)
    
    member __.Commit (message: string) =
      IO.delay <|
        fun () ->
          printfn "Actually commit %s" message
          let author = { Signature.name = "Test"; email = "test@yolodev" }
          
          repo.Commit (message, author, author) 
          |> IO.map CommitWrapper.ofCommit
    
    member __.Tag (name: string) =
      IO.delay <|
        fun () ->
          printfn "Actually tag %s" name

          repo.ApplyTag name
          |> IO.map TagWrapper.ofTag

  interface IDisposable with
    member __.Dispose () = 
      (repo :> System.IDisposable).Dispose ()
      match dispose with
      | Some d -> d ()
      | _      -> ()
