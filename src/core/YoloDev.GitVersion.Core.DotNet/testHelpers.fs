module YoloDev.GitVersion.Core.TestHelpers

open System.IO
open LibGit2Sharp
open YoloDev.GitVersion.Core.Abstractions
open YoloDev.GitVersion.Core.DotNet.Types

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
    let dir = Helpers.mkTmpDir ()
    let p = Repository.Init dir.FullName
    let repo = new Repository (p)
    new RepositoryWrapper (repo, fun () -> dir.Delete true)
    :> IRepository
    |> Repo.from
