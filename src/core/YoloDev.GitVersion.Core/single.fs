module YoloDev.GitVersion.SingleRepo

open YoloDev.GitVersion.Core
//open YoloDev.GitVersion.Core.Operators.IO


[<RequireQualifiedAccess>]
module Version =

  let info = SingleVersion.versionInfo
  
  let newReleaseVersion repo =
    io {
      let! info = info repo

      match info.commitsSinceVersion, info.prevVersion with
      | 0, Some v -> return v
      | _, _ ->
        return
          match info.prevVersion, info.change with
          | None, Change.Major -> Semver.parse "1.0.0"
          | None, _            -> Semver.parse "0.1.0"
          | Some v, c          -> Change.addTo v c
    }

  let currentVersion repo =
    io {
      let! info = info repo

      match info.commitsSinceVersion, info.prevVersion with
      | 0, Some v -> return v
      | n, _ ->
        let nextFullVersion =
          match info.prevVersion, info.change with
          | None, Change.Major -> Semver.parse "1.0.0"
          | None, _            -> Semver.parse "0.1.0"
          | Some v, c          -> Change.addTo v c
        
        match info.branch with
        | "master" -> return { nextFullVersion with pre = [Str "ci"; Num n ] }
        // TODO: This *needs* to be improved
        | branch   -> 
          let shaStr = info.sha |> Option.map Hash.string7 |> Option.defaultValue "EMPTY"
          let branchPart = branch.Split [|'/'|] |> Seq.last
          let pre = [Str branchPart; Num n]
          let build = 
            if info.dirty 
            then [Str shaStr; Str "DIRTY"]
            else [Str shaStr]
          
          return { nextFullVersion with pre = pre; build = build }
    }
