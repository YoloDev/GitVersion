// Learn more about F# at http://fsharp.org

open System.IO

open YoloDev.GitVersion.Core
open YoloDev.GitVersion
open YoloDev.GitVersion.SingleRepo

[<EntryPoint>]
let main argv =
  let dir =
    match Array.tryHead argv with
    | Some dir -> dir
    | None     -> Directory.GetCurrentDirectory ()
  
  printfn "Dir: %s" dir

  io {
    let! repo = Repo.openExisting dir
    return! Version.prevVersion repo
  }
  |> IO.run
  |> Async.RunSynchronously
  |> Option.defaultValue (Semver.parse "0.1.0")
  |> printfn "Prev version: %A"

  Repo.openExisting dir
  |> IO.bind Version.newReleaseVersion
  |> IO.run
  |> Async.RunSynchronously
  |> printfn "New tag version: %A"

  Repo.openExisting dir
  |> IO.bind Version.currentVersion
  |> IO.run
  |> Async.RunSynchronously
  |> printfn "Current version: %A"
  
  0 // return an integer exit code
