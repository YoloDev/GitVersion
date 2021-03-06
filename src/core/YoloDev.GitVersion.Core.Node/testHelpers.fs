module YoloDev.GitVersion.Core.TestHelpers

open YoloDev.GitVersion.Core.Node.Shim
open YoloDev.GitVersion.Core.Node.Types
open YoloDev.GitVersion
open YoloDev.GitVersion.Core.Logging
open YoloDev.GitVersion.Core.Abstractions
open Fable.Core
open Fable.PowerPack
open Fable.Import.Node
open Bindings.NodeGit

let logger = Logger.create "YoloDev.GitVersion.Core.TestHelpers"

[<RequireQualifiedAccess>]
module internal Helpers =
  let rec mkTmpDir () =
    Promise.create <| fun resolve reject ->
      fs.mkdtemp ((path.join [| os.tmpdir (); "gitversion-" |]), fun err path ->
        match err with
        | None -> resolve path
        | Some e -> reject (e :?> exn))
  
  let rec deleteDirRec (dir: string) =
    if fs.existsSync (U2.Case1 dir) then
      for file in fs.readdirSync (U2.Case1 dir) do
        let curPath = path.join [| dir; file |]
        if (fs.lstatSync (U2.Case1 curPath)).isDirectory ()
        then deleteDirRec curPath
        else fs.unlinkSync (U2.Case1 curPath)
      
      fs.rmdirSync (U2.Case1 dir)

  let postDelete path =
    fun () -> 
      deleteDirRec path
      Logger.verbose logger (
              eventX "Deleted test repository at {path}"
              >> setField "path" path)
      |> IO.run
      |> Promise.start

[<RequireQualifiedAccess>]
module Repo =
  let createTestRepo () =
      IO.ofPromiseFactory <| fun () ->
        Helpers.mkTmpDir ()
        |> Promise.bind (fun path ->
          nodegit.Repository.init path (0 (* false *))
          |> Promise.bind (fun r -> 
            Logger.verbose logger (
              eventX "Created test repository at {path}"
              >> setField "path" path)
            |> IO.run
            |> Promise.map (fun () -> new Node.Shim.Repository (r)))          
          |> Promise.map (fun repo -> new RepositoryWrapper (repo, Helpers.postDelete path) :> IRepository))
    |> IO.bind Repo.from
