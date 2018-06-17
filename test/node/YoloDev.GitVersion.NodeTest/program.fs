module YoloDev.GitVersion.NodeTest

open YoloDev.GitVersion
open YoloDev.GitVersion.Common
open YoloDev.GitVersion.Core.Logging
open YoloDev.GitVersion.Core.TestHelpers
open Fable.Core
open Fable.Import.Node
open Fable.PowerPack
open System

let logger = Logger.create "YoloDev.GitVersion.NodeTest"

module Helpers =

  [<Emit("String($0.stack || $0)")>]
  let errorStr (_: exn) : string = jsNative

  [<Emit("debugger")>]
  let debugger () : unit = jsNative

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

let exitSuccess (n: int) =
  Globals.``process``.exit n

let exitError (e: exn) =
  printfn "Error: %s" (Helpers.errorStr e)
  Globals.``process``.exit 1

mainIO (Globals.``process``.argv |> Seq.skip 2 |> List.ofSeq)
|> IO.run
|> Promise.eitherEnd exitSuccess exitError
