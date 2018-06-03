[<AutoOpen>]
module YoloDev.GitVersion.DotNetRun

open System
open LibGit2Sharp
open YoloDev.GitVersion.Core.Abstractions
open YoloDev.GitVersion.Core.DotNet.Types

[<Struct>]
type private SystemImpl =

  interface ISystem with
    member __.GetEnv n = IO.delay <| fun () ->
      match Environment.GetEnvironmentVariable n with
      | null -> IO.unit None
      | s    -> IO.unit (Some s)
    
    member __.OpenRepository p = IO.delay <| fun () ->
      IO.unit (new RepositoryWrapper (new Repository (p)) :> IRepository)
    
    member __.GetLogger name = IO.delay <| fun () ->
      IO.unit (new LoggerWrapper (Logary.Facade.Log.create name) :> ILogger)

[<RequireQualifiedAccess>]
module IO =

  let run (ma: #IO<_>) =
    Async.FromContinuations <|
      fun (cok, cerr, _) ->
        IO.fork ma (SystemImpl ()) <|
          function
          | Ok a -> 
            cok a
            FakeUnit
          | Error e ->
            cerr e
            FakeUnit
        |> ignore
