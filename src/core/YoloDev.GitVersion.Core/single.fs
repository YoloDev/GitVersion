module YoloDev.GitVersion.SingleRepo

open YoloDev.GitVersion.Core
open YoloDev.GitVersion.Core.Operators.IO

[<RequireQualifiedAccess>]
module Version =

  let prevVersion =
    SingleVersion.prevTag >-> Option.map VersionTag.version
  
  let newReleaseVersion repo =
    SingleVersion.prevTag repo >>=
      function
      | None -> IO.unit (Some (Semver.parse "0.1.0"))
      | Some versionTag ->
        let tag = VersionTag.tag versionTag
        SingleVersion.diff tag repo <.>
          function
          | SameCommit -> None
          | DiffCommits c ->
            let prevVersion = VersionTag.version versionTag
            Change.addTo prevVersion c |> Some
  
  let currentVersion repo =
    io {
      let! prevVersion = SingleVersion.prevTag repo
      let! nextRelease, sameCommit =
        match prevVersion with
        | None -> IO.unit (Semver.parse "0.1.0", false)
        | Some versionTag ->
          let tag = VersionTag.tag versionTag
          let prevVersion = VersionTag.version versionTag
          SingleVersion.diff tag repo <.>
            function
            | SameCommit -> prevVersion, true
            | DiffCommits c -> Change.addTo prevVersion c, false
      
      if sameCommit
      then return nextRelease
      else
        let! releaseMeta = SingleVersion.prerelease (Option.map VersionTag.tag prevVersion) repo
        return
          match releaseMeta with
          | MasterPre n -> { nextRelease with pre = [Str "ci"; Num n] }
          | BranchPre (name, hash, n) -> { nextRelease with pre = [Str name; Num n]; build = [Str (Hash.string hash)] }
          | PullRequestPre (id, hash, n) -> { nextRelease with pre = [Str "pr"; Num id; Num n]; build = [Str (Hash.string hash)] }
    }
