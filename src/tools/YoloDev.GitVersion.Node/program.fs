module YoloDev.GitVersion.Node

open YoloDev.GitVersion.Core
open YoloDev.GitVersion
open YoloDev.GitVersion.SingleRepo
open Fable.Import.Node
open Fable.PowerPack
open Fable.Core

module Helpers =
  [<Emit("String($0.stack || $0)")>]
  let errorStr (_: exn) : string = jsNative

  [<Emit("debugger;")>]
  let debug () = jsNative

let main argv =
  let dir =
    match List.tryHead argv with
    | Some dir -> dir
    | None     -> Globals.``process``.cwd ()
  
  printfn "Dir: %s" dir

  Repo.openExisting dir
  // |> IO.bind Version.prevVersion
  // |> IO.map (Option.defaultValue (Semver.parse "0.1.0"))
  // |> IO.map (printfn "Prev version: %O")
  |> IO.bind (fun _ ->
    Repo.openExisting dir
    |> IO.bind Version.newReleaseVersion
    |> IO.map (printfn "New tag version: %O")
    |> IO.bind (fun _ ->
      Repo.openExisting dir
      |> IO.bind Version.currentVersion
      |> IO.map (printfn "Current version: %O")))
  |> IO.run

main (Globals.``process``.argv |> Seq.skip 2 |> List.ofSeq)
|> Promise.tryStart (fun e ->
  printfn "Error: %s" (Helpers.errorStr e)
  Globals.``process``.exit 1)