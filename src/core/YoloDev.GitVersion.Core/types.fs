[<AutoOpen>]
module YoloDev.GitVersion.Types

open YoloDev.GitVersion.Core

type Change =
  | Patch
  | Minor
  | Major

[<RequireQualifiedAccess>]
module Change =

  let inline addTo version =
    function
    | Patch    -> 
      { version with 
          patch = version.patch + 1
          pre = []
          build = [] }
    | Minor    ->
      { version with 
          minor = version.minor + 1
          patch = 0
          pre = []
          build = [] }
    | Major    ->
      { version with 
          major = version.major + 1
          minor = 0
          patch = 0
          pre = []
          build = [] }

type VersionDiff =
  | SameCommit
  | DiffCommits of Change

type Prerelease =
  | MasterPre of commitCount: int // commits since release tag
  | BranchPre of name: string * hash: Hash * commitCount: int // commits since master
  | PullRequestPre of id: int * hash: Hash * commitCount: int // commits in PR

[<Struct>]
type VersionTag = 
  internal { tag : Tag
             version : Semver }

[<RequireQualifiedAccess>]
module VersionTag =
  
  let version tv = tv.version

  let internal tag tv = tv.tag