namespace rec YoloDev.GitVersion.Core.Abstractions

open System.Runtime.CompilerServices

type
  #if !NODE
  internal 
  #endif 
  FakeUnit = FakeUnit

type 
  #if !NODE
  internal 
  #endif 
  Cont<'t> = Result<'t, exn> -> FakeUnit

type 
  #if !NODE
  internal 
  #endif 
  Next<'t> = 't -> Cont<bool> -> FakeUnit

type 
  #if !NODE
  internal 
  #endif 
  Done = Result<unit, exn> -> FakeUnit

type 
  #if !NODE
  internal 
  #endif 
  ISystem =
    abstract member GetEnv: string -> IO<string option>
    abstract member OpenRepository: string -> IO<IRepository>

type 
  #if !NODE
  internal 
  #endif 
  IReference =
    inherit System.IDisposable
    abstract member Sha: IO<string>

type 
  #if !NODE
  internal 
  #endif 
  ICommit =
    inherit IReference
    abstract member Message: IO<string>

type 
  #if !NODE
  internal 
  #endif 
  ITag =
    inherit IReference
    abstract member Name: IO<string>
    abstract member Commit: IO<ICommit>

type 
  #if !NODE
  internal 
  #endif 
  IBranch =
    inherit System.IDisposable
    abstract member Name: IO<string>
    abstract member Tip: IO<ICommit option>

type 
  #if !NODE
  internal 
  #endif 
  ICommitFilter =
    abstract member ExcludeReachableFrom: string list
    abstract member IncludeReachableFrom: string list
    abstract member FirstParentOnly: bool

type 
  #if !NODE
  internal 
  #endif 
  ICommitLog =
    inherit System.IDisposable
    abstract member Seq: IOSeq<ICommit>
    abstract member Query: ICommitFilter -> IOSeq<ICommit>

type 
  #if !NODE
  internal 
  #endif 
  IRepository =
    inherit System.IDisposable
    abstract member Tags: IOSeq<ITag>
    abstract member Commits: ICommitLog
    abstract member Head: IO<IBranch>
    abstract member Commit: string -> IO<ICommit>
    abstract member Tag: string -> IO<ITag>

[<NoEquality; NoComparison>]
type IO<'t> 
  #if !NODE
  internal 
  #endif 
  (fork: ISystem -> Cont<'t> -> FakeUnit) =
    member internal __.Fork sys cont = fork sys cont

[<NoEquality; NoComparison>]
type IOSeq<'t> 
  #if !NODE
  internal 
  #endif 
  (forkSeq: ISystem -> Next<'t> -> Done -> FakeUnit) =
    inherit IO<'t seq> (fun sys cont ->
      let mutable items = []
      let onNext item cont' =
        items <- item :: items
        cont' (Ok true)
      let onDone r =
        match r with
        | Ok () -> items |> Seq.rev |> Ok |> cont
        | Error e -> cont (Error e)
      
      forkSeq sys onNext onDone)
    
    member internal __.ForkSeq sys next cont = forkSeq sys next cont

[<assembly: InternalsVisibleToAttribute("YoloDev.GitVersion.Core.DotNet")>]
[<assembly: InternalsVisibleToAttribute("YoloDev.GitVersion.Core.Node")>]
do ()