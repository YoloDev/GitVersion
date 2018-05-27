module YoloDev.GitVersion.NodeTest

open YoloDev.GitVersion
open YoloDev.GitVersion.Common
open YoloDev.GitVersion.Core.TestHelpers
open Fable.Core
open Fable.Import.Node
open Fable.PowerPack
open System

module Helpers =

  [<Emit("String($0.stack || $0)")>]
  let errorStr (_: exn) : string = jsNative

  [<Emit("debugger")>]
  let debugger () : unit = jsNative

  let readFile path =
    Promise.create <| fun resolve reject ->
      fs.readFile (path, "utf-8", fun err content ->
        match err with
        | None -> resolve content
        | Some e -> reject (e :> obj :?> exn))
  
  let readAllLines =
    readFile
    >> Promise.map (fun s -> s.Split ([|'\n'|], StringSplitOptions.RemoveEmptyEntries))
    >> Promise.map (Seq.map (fun s -> s.Trim ()))
    >> Promise.map List.ofSeq

let main argv =
  Helpers.readAllLines "cases/simple/01.case"
  |> Promise.map (TestCase.parse "simple/01.case")
  |> Promise.tap (string >> printfn "%s")
  |> Promise.bind (fun test ->
    Repo.createTestRepo ()
    |> IO.bind (fun repo -> TestCase.run repo test)
    |> IO.tap (fun _ -> printfn "!!! Done evaluating test")
    |> IO.run)

main (Globals.``process``.argv |> Seq.skip 2 |> List.ofSeq)
|> Promise.tryStart (fun e ->
  printfn "Error: %s" (Helpers.errorStr e)
  Globals.``process``.exit 1)