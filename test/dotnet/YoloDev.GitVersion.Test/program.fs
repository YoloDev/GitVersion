// Learn more about F# at http://fsharp.org

open YoloDev.GitVersion
open YoloDev.GitVersion.Common
open YoloDev.GitVersion.Core.Logging
open YoloDev.GitVersion.Core.TestHelpers
open Logary.Configuration
open Logary.Targets
open Logary.Adapters.Facade

let logger = Logger.create "YoloDev.GitVersion.NodeTest"

let tryRun testCase =
  io {
    use! repo = Repo.createTestRepo ()
    try 
      do! TestCase.run repo testCase
      return 0
    with e ->
      do! Logger.error logger (
            eventX "Test case failed: {case}"
            >> setField "case" testCase.name
            >> addExn e)
      return 1
  }

let mainIO _ =
  Tests.discover "cases"
  |> IOSeq.mapM tryRun
  |> IO.map Seq.sum

[<EntryPoint>]
let main argv =
  let logary =
    Config.create "YoloDev.GitVersion.Test" "localhost"
    |> Config.target (LiterateConsole.create LiterateConsole.empty "console")
    |> Config.loggerMinLevel "YoloDev.*" Logary.LogLevel.Verbose
    |> Config.buildAndRun
  
  LogaryFacadeAdapter.initialise<YoloDev.GitVersion.Logary.Facade.Logger> logary

  mainIO argv
  |> IO.run
  |> Async.RunSynchronously
