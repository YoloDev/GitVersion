[<AutoOpen>]
module YoloDev.GitVersion.Core.Git

open System
open YoloDev.GitVersion.Core.Abstractions
open YoloDev.GitVersion

[<Struct; NoEquality; NoComparison>]
type Repo = private Repo of IRepository with

  interface IDisposable with
    member x.Dispose () = 
      let (Repo r) = x
      r.Dispose ()

[<Struct; NoEquality; NoComparison>]
type Tag = 
  #if !NODE
  private 
  #endif
    Tag of ITag with

    interface IDisposable with
      member x.Dispose () = 
        let (Tag t) = x
        t.Dispose ()

[<Struct; NoEquality; NoComparison>]
type Commit = private Commit of ICommit with

  interface IDisposable with
    member x.Dispose () = 
      let (Commit c) = x
      c.Dispose ()

[<Struct; NoEquality; NoComparison>]
type Branch = private Branch of IBranch with

  interface IDisposable with
    member x.Dispose () = 
      let (Branch b) = x
      b.Dispose ()

[<Struct>]
type Hash =
  #if !NODE
  private 
  #endif
    Hash of string

[<RequireQualifiedAccess>]
module Hash =

  let initial = Hash "INITIAL"
  let string (Hash h) = h.ToLowerInvariant ()
  let string7 h = (string h).Substring (0, 7)

type CommitFilter =
  private {
    includeReachableFrom: string list
    excludeReachableFrom: string list
    firstParentOnly: bool
  }

  interface ICommitFilter with
    member x.IncludeReachableFrom = x.includeReachableFrom
    member x.ExcludeReachableFrom = x.excludeReachableFrom
    member x.FirstParentOnly = x.firstParentOnly

module CommitFilter =
  let revWalk = {
    includeReachableFrom = ["HEAD"]
    excludeReachableFrom = []
    firstParentOnly = false
  }

  let withIncludeReachableFrom hs f =
    { f with includeReachableFrom = List.map (fun (Hash h) -> h) hs }
  
  let withExcludeReachableFrom hs f =
    { f with excludeReachableFrom = List.map (fun (Hash h) -> h) hs }
  

  let withExcludeReachableFromMaster f =
    { f with excludeReachableFrom = ["master"] }
  
  let withFirstParentOnly f =
    { f with firstParentOnly = true }

type CommitLog internal (log: ICommitLog) =
  inherit IOSeq<Commit> (IOSeq.forkSeq (log.Seq |> IOSeq.map Commit))

  member internal __.Get (sha: string) = log.Get sha |> IO.map (Option.map Commit)

  member internal __.Query (filter: CommitFilter) =
    log.Query filter
    |> IOSeq.map Commit

module CommitLog =

  let query f (l: CommitLog) = l.Query f

  let get (Hash h) (l: CommitLog) = l.Get h

[<RequireQualifiedAccess>]
module Repo =

  let internal from r = IO.unit (Repo r)

  let openExisting path = IO <| fun sys cont ->
    IO.fork (sys.OpenRepository path) sys (Result.map Repo >> cont)

  let tags (Repo r) =
    r.Tags |> IOSeq.map Tag
  
  let commits (Repo r) =
    CommitLog r.Commits
  
  let head (Repo r) =
    r.Head |> IO.map Branch
  
  let commit s (Repo r) =
    r.Commit s |> IO.map Commit
  
  let tag s (Repo r) =
    r.Tag s |> IO.map Tag
  
  let isDirty (Repo r) =
    r.IsDirty
  
  let createBranch n (Repo r) =
    r.CreateBranch n
    |> IO.bind r.Checkout

[<RequireQualifiedAccess>]
module Commit =

  let hash (Commit c) = c.Sha |> IO.map Hash

  let message (Commit c) = c.Message

[<RequireQualifiedAccess>]
module Tag =

  let commit (Tag t) = t.Commit |> IO.map Commit

  let hash t = commit t |> IO.bind Commit.hash

  let name (Tag t) = t.Name


[<RequireQualifiedAccess>]
module Branch =

  let name (Branch b) =
    b.Name
  
  let tip (Branch b) =
    b.Tip |> IO.map (Option.map Commit)
