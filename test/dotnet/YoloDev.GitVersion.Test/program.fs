// Learn more about F# at http://fsharp.org

open YoloDev.GitVersion
open YoloDev.GitVersion.Common
open YoloDev.GitVersion.Core.TestHelpers
open Logary.Configuration
open Logary.Targets
open Logary.Adapters.Facade

[<EntryPoint>]
let main argv =
  let logary =
    Config.create "YoloDev.GitVersion.Test" "localhost"
    |> Config.target (LiterateConsole.create LiterateConsole.empty "console")
    |> Config.loggerMinLevel "YoloDev.*" Logary.LogLevel.Verbose
    |> Config.buildAndRun
  
  LogaryFacadeAdapter.initialise<YoloDev.GitVersion.Core.Logging.Logger> logary

  let test = 
    System.IO.File.ReadAllLines "cases/simple/01.case"
    |> List.ofArray
    |> TestCase.parse "simple/01.case"

  io {
    use! repo = Repo.createTestRepo ()
    do! TestCase.run repo test
  }
  |> IO.run
  |> Async.RunSynchronously

  0 // return an integer exit code
