// Learn more about F# at http://fsharp.org

open YoloDev.GitVersion
open YoloDev.GitVersion.Common
open YoloDev.GitVersion.Core.TestHelpers

[<EntryPoint>]
let main argv =
  let test = 
    System.IO.File.ReadAllLines "cases/simple/01.case"
    |> List.ofArray
    |> TestCase.parse "simple/01.case"
  printfn "%A" test

  io {
    let! repo = Repo.createTestRepo ()
    do! TestCase.run repo test
  }
  |> IO.run
  |> Async.RunSynchronously

  0 // return an integer exit code
