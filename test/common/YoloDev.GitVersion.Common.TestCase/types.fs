[<AutoOpen>]
module YoloDev.GitVersion.Common.TestCase

open YoloDev.GitVersion
open YoloDev.GitVersion.Core
open System

let logger = Logger.create "YoloDev.GitVersion.Common.TestCase"

[<AutoOpen>]
module internal Helpers =

#if NODE
  open Fable.Core

  [<Emit("debugger;")>]
  let debugger () : unit = jsNative

#else
  let debugger () : unit = ()
#endif

[<AutoOpen>]
module private Parsing =
  let tokens (origStr: string) =
    let tokens = System.Collections.Generic.List ()
    let mutable str = origStr
    while String.length str > 0 do
      if str.[0] = '"'
      then
        let endQuote = str.IndexOf ('"', 1)
        if endQuote = -1 then failwithf "Invalid line: %s" origStr
        let part = str.Substring (1, endQuote - 1)
        tokens.Add part
        str <- str.Substring (endQuote + 1)
      else
        let space = str.IndexOf ' '
        if space = -1 
        then
          tokens.Add str
          str <- ""
        else
          let part = str.Substring (0, space)
          tokens.Add part
          str <- str.Substring (space + 1)
    
    tokens
    |> Seq.map (fun s -> s.Trim ())
    |> Seq.filter (fun s -> s.Length > 0)
    |> List.ofSeq
  
  let (|Semver|_|) = Semver.tryParse

type TestStep =
  | Expect of version: Semver
  | Commit of message: string
  | CreateBranch of branch: string
  | Release

  override step.ToString () =
    match step with
    | Expect v       -> sprintf "Expect %s" (string v)
    | Commit m       -> sprintf "Commit %s" m
    | CreateBranch b -> sprintf "Create branch %s" b
    | Release        -> sprintf "Release"

[<RequireQualifiedAccess>]
module TestStep =

  let private replaceTokens (v: Semver) (repo: Repo) =
    let chooseM f l =
      IOSeq.ofSeq l
      |> IOSeq.chooseM f
      |> IO.map List.ofSeq

    let replaceToken =
      function
      | Str "SHA7" -> io {
        let! head = Repo.head repo
        let! commit = Branch.tip head
        match commit with
        | None -> return Some (Str "EMPTY")
        | Some commit ->
          let! hash = Commit.hash commit
          return Some (Str (Hash.string7 hash)) }
      
      | segment -> IO.unit (Some segment)

    io {
      let! pre = chooseM replaceToken v.pre
      let! build = chooseM replaceToken v.build
      return { v with pre = pre; build = build }
    }

  let parse name index =
    tokens >> function
      | ["expect"; Semver v]    -> Expect v
      | ["commit"]              -> Commit (sprintf "Commit for line %d" index)
      | ["commit"; msg]         -> Commit (sprintf "Commit for line %d\n\n%s" index msg)
      | ["create-branch"; name] -> CreateBranch name
      | ["release"]             -> Release
      | tokens                  -> failwithf "Unknown tokens in file %s: %A" name tokens
  
  let evaluate repo name index =
    function
    | Expect expected ->
      io {
        let! expected = replaceTokens expected repo
        let! actual = YoloDev.GitVersion.SingleRepo.Version.currentVersion repo
        if expected <> actual then
          failwithf "Step %d at %s: Expected %A to be %A" index name actual expected
      }
    
    | Commit m ->
      IO.combine (Repo.commit m repo) IO.zero
    
    | CreateBranch b ->
      IO.combine (Repo.createBranch b repo) IO.zero
    
    | Release ->
      YoloDev.GitVersion.SingleRepo.Version.newReleaseVersion repo
      |> IO.bind (fun v -> IO.combine (Repo.tag (sprintf "v%A" v) repo) IO.zero)

type TestKind =
  | Simple

  override kind.ToString () =
    match kind with
    | Simple -> "Simple"

type TestCase = 
  { name: string
    kind: TestKind
    steps: TestStep list }
  
  override case.ToString () =
    sprintf "%s - %s\n%s" case.name (string case.kind) (String.concat "\n" <| List.map string case.steps)


[<RequireQualifiedAccess>]
module TestCase =

  let private (|Kind|_|) str =
    match tokens str with 
    | ["#simple"] -> Some Simple
    | _           -> None

  let parse name lines =
    match lines with
    | Kind k :: steps ->
      { name = name
        kind = k
        steps = List.mapi (fun n -> TestStep.parse name (n + 2)) steps }
    | _ -> failwithf "Could not parse test-case %s" name
  
  let run repo case =
    io {
      do! Logger.info logger (
            eventX "Starting test case {case}"
            >> setField "case" case.name)

      for index, step in IOSeq.coerce (Seq.indexed case.steps) do
        do! Logger.info logger (
              eventX "Evaluate {step}"
              >> setField "step" step)
        
        do! TestStep.evaluate repo case.name index step
        do! Logger.info logger (
              eventX "Done evaluating {step}"
              >> setField "step" step)
      
      do! Logger.info logger (
            eventX "Done running test case {case}"
            >> setField "case" case.name)
    }

[<RequireQualifiedAccess>]
module Tests =
  let rec private findFiles dir path =
    ioSeq {
      let! dir = Fs.getDir dir
      for entry in Dir.entries dir do
        if Entry.isDir entry then yield! findFiles (Entry.path entry) (Entry.name entry :: path)
        elif Entry.isFile entry then yield (path, File.ofEntry entry)
    }
  
  let private parse (path, file) =
    io {
      let! content = File.read file
      let lines = 
        content.Split ([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun s -> s.Trim ())
        |> List.ofSeq
      
      let name = String.concat "/" (List.rev (Entry.name file :: path))
      return TestCase.parse name lines
    }
  
  let discover dir =
    findFiles dir []
    |> IOSeq.mapM parse
