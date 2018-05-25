[<AutoOpen>]
module internal YoloDev.GitVersion.Core.Version

open System.Text.RegularExpressions
open YoloDev.GitVersion

type Branch =
  | Master
  | Branch of name: string
  | PullRequest of number: int

[<RequireQualifiedAccess>]
module Ci =

  let pullRequest =
    let toPr = IO.map (Option.bind Int.tryParse >> Option.map PullRequest)
    IO.choose [
      Env.getVar "APPVEYOR_PULL_REQUEST_NUMBER" |> toPr
      Env.getVar "TRAVIS_PULL_REQUEST" |> toPr
    ]
  
  let branch =
    let toBranch = IO.map (Option.map <| 
                    function | "master" -> Master 
                             | b -> Branch (Regex.replace "\\." "-" b))

    IO.choose [
      Env.getVar "APPVEYOR_REPO_BRANCH" |> toBranch
      Env.getVar "TRAVIS_BRANCH" |> toBranch
    ]

[<RequireQualifiedAccess>]
module Repo =

  let branchName repo =
    let toBranch = IO.map (function | "master" -> Master 
                                    | b -> Branch (Regex.replace "\\." "-" b))

    IO.choose [
      Ci.pullRequest
      Ci.branch
    ] |> IO.bind (function
      | Some branch -> IO.unit branch
      | None ->
        Repo.head repo
        |> IO.bind Branch.name
        |> toBranch)

[<RequireQualifiedAccess>]
module Branch =

  let distanceFromMaster repo =
    Repo.commits repo
    |> CommitLog.query (CommitFilter.revWalk |> CommitFilter.withExcludeReachableFromMaster)
    |> IOSeq.count

[<RequireQualifiedAccess>]
module Tag =

  let singleVersionTag =
    let parser = Semver.mkParser (Regex.replace "^\^" "^v") (fun _ v -> Ok v)

    fun tag ->
      io {
        let! name = Tag.name tag
        match parser name with
        | Ok v -> return (Some { tag = tag; version = v })
        | _    -> return None
      }
  
  let versionTagForName name =
    let parser = Semver.mkParser (Regex.replace "^\^" (sprintf "^%s" <| Regex.escape name)) (fun _ v -> Ok v)

    fun tag ->
      io {
        let! name = Tag.name tag
        match parser name with
        | Ok v -> return (Some { tag = tag; version = v })
        | _    -> return None
      }


[<RequireQualifiedAccess>]
module Commit =

  let differentThan hash =
    Commit.hash >> IO.map ((=) hash)
  
  let semverMessage prefix =
    let regex =
      let prefixStr = Regex.escape (sprintf "%s:" prefix)
      let regexStr = sprintf "^ *%s *(minor|major) *$" prefixStr
      Regex (regexStr, 
        #if !NODE
        RegexOptions.CultureInvariant |||
        #endif
        RegexOptions.IgnoreCase ||| RegexOptions.Multiline)
    
    fun message ->
      match regex.Match message with
      | null   -> None
      | m ->
        let group = m.Groups.[1]
        match group.Value.ToLowerInvariant () with
        | "minor" -> Some Minor
        | "major" -> Some Major
        | _       -> None

[<RequireQualifiedAccess>]
module SingleVersion =

  let prevTag repo =
    Repo.tags repo
    |> IOSeq.chooseM Tag.singleVersionTag
    |> IOSeq.tryHead
  
  let diff tag repo =
    io {
      let commits = Repo.commits repo
      let! head = IOSeq.tryHead commits
      let! tagHash = Tag.hash tag

      let! isSame =
        match head with
        | None   -> IO.unit false
        | Some c -> 
          Commit.hash c
          |> IO.map ((=) tagHash)
      
      if isSame then 
        return SameCommit
      else
        return!
          commits
          |> CommitLog.query (CommitFilter.revWalk |> CommitFilter.withExcludeReachableFrom [tagHash])
          |> IOSeq.chooseM (Commit.message >> IO.map (Commit.semverMessage "Semver"))
          |> IOSeq.tryMax
          |> IO.map ((Option.defaultValue Patch) >> DiffCommits)
    }
  

  let prerelease tag repo =
    io {
      let! branchName = Repo.branchName repo
      let! tip = Repo.head repo |> IO.bind Branch.tip
      let! branchHash =
        match tip with
        | None -> IO.unit Hash.initial
        | Some commit -> Commit.hash commit
        
      let! filter =
        match tag with
        | None -> IO.unit CommitFilter.revWalk
        | Some tag -> 
          Tag.hash tag
          |> IO.map (fun tagHash -> CommitFilter.revWalk |> CommitFilter.withExcludeReachableFrom [tagHash])
      match branchName with
      | Master ->
        return!
          Repo.commits repo
          |> CommitLog.query filter
          |> IOSeq.count
          |> IO.map MasterPre
      | Branch name ->
        return!
          Branch.distanceFromMaster repo
          |> IO.map (fun count -> BranchPre (name, branchHash, count))
      | PullRequest n ->
        return!
          Branch.distanceFromMaster repo
          |> IO.map (fun count -> PullRequestPre (n, branchHash, count))
    }

// [<RequireQualifiedAccess>]
// module MonoRepoVersion =
  
//   let prevTags names =
//     match names with
//     | [] -> Git.unit Map.empty
//     | _ ->
//       git {
//         let! tags = Repo.tags

//         return
//           tags
//           |> Seq.combineAll Tag.versionTagForName names
//           |> Map.ofSeq
//       }
  
//   let diffs =
//     let rec runFilter commitHash commitMessage changes counts index remaining acc =
//       match remaining with
//       | [] -> changes, acc // order does not matter
//       | h::t ->
//         let (name, hash, parser) = h

//         if hash = commitHash
//         then 
//           let counts = Map.add name index counts
//           runFilter commitHash commitMessage changes counts index t acc

//         else
//           let changeKind = parser commitMessage
//           let prev = Map.find name changes
//           let changes =
//             match changeKind with
//             | Some change when change > prev -> Map.add name change changes
//             | _                              -> changes
          
//           runFilter commitHash commitMessage changes counts index t (h::acc)

//     fun tags ->
//       git {
//         let! commits = Repo.commits
//         let changes = Map.map (fun _ _ -> Patch) tags
//         let counts = Map.empty
//         let remaining = 
//           tags 
//           |> Map.toSeq 
//           |> Seq.map (fun (name, tag) -> name, Tag.hash tag, Commit.semverMessage name) 
//           |> List.ofSeq
//         let state = (changes, counts, remaining)

//         let folder (changes, counts, remaining) index commit =
//           let commitHash = Commit.hash commit
//           let commitMessage = Commit.message commit
          
//           let changes, remaining = runFilter commitHash commitMessage changes counts index remaining []
//           (changes, counts, remaining), List.isEmpty remaining
        
//         let (changes, counts, remaining) = Seq.foldsi folder state commits
        
//         return 
//           if not (List.isEmpty remaining) 
//           then failwithf "Remaining list was not empty"
//           else
//             counts
//             |> Map.map (fun name -> 
//               function | 0 -> SameCommit 
//                        | n -> DiffCommits (n, Map.find name changes))
//       }