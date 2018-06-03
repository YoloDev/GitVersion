module YoloDev.GitVersion.Core.TestHelpers

open System.IO
open LibGit2Sharp
open YoloDev.GitVersion
open YoloDev.GitVersion.Core.Abstractions
open YoloDev.GitVersion.Core.DotNet.Types
open YoloDev.GitVersion.Core.Logging

let logger = Logger.create "YoloDev.GitVersion.Core.TestHelpers"

[<RequireQualifiedAccess>]
module internal Helpers =
  let tmpPath = Path.GetTempPath ()
  let rec mkTmpDir () =
    let tmpDir = Path.Combine (tmpPath, Path.GetRandomFileName ())
    if File.Exists tmpDir || Directory.Exists tmpDir
    then mkTmpDir ()
    else Directory.CreateDirectory tmpDir

[<RequireQualifiedAccess>]
module Repo =
  let createTestRepo () =
    io {
      let dir = Helpers.mkTmpDir ()
      let p = Repository.Init dir.FullName
      let repo = new Repository (p)
      do! Logger.verbose logger (
            eventX "Created test repository at {path}"
            >> setField "path" dir.FullName)
      
      let cleanup () =
        io {
          do! IO.waitForFS
          do! Logger.verbose logger (
                eventX "Delete directory {path}"
                >> setField "path" dir.FullName)
          try dir.Delete true
          with e ->
            do! Logger.verbose logger (
                  eventX "Failed to delete directory {path}"
                  >> setField "path" dir.FullName
                  >> addExn e)
        } |> IO.run |> Async.Start

      let wrapper = new RepositoryWrapper (repo, cleanup) :> IRepository
      
      return! Repo.from wrapper
    }
