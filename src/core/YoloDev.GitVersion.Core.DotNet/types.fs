module internal YoloDev.GitVersion.Core.DotNet.Types

open System
open LibGit2Sharp
open YoloDev.GitVersion.Core.Abstractions
open YoloDev.GitVersion.SystemBuilders

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
      IO.delay <|
        fun () ->
          printfn "Actually commit %s" message
          let author = Signature ("Test", "@yolodev", DateTimeOffset.Now)
          
          repo.Commit (message, author, author, CommitOptions (AllowEmptyCommit = true)) 
          |> CommitWrapper.ofCommit
          |> IO.unit
    
    member __.Tag (name: string) =
      IO.delay <|
        fun () ->
          printfn "Actually tag %s" name

          repo.ApplyTag name
          |> TagWrapper.ofTag
          |> IO.unit
    
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
