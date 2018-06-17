[<AutoOpen>]
module YoloDev.GitVersion.Core.FileSystem

open YoloDev.GitVersion.Core.Abstractions
open YoloDev.GitVersion

type Entry internal (e: IFileSystemEntry) =
  member internal __.Entry = e

type Dir internal (d: IDirectory) =
  inherit Entry (d)
  member internal __.Dir = d

type File internal (f: IFile) =
  inherit Entry (f)
  member internal __.File = f

[<RequireQualifiedAccess>]
module Entry =
  let inline internal (|Entry|) (e: Entry) = e.Entry

  let internal ofEntry (e: IFileSystemEntry) =
    match e with
    | :? IDirectory as d when e.IsDirectory -> Dir d :> Entry
    | :? IFile as f      when e.IsFile      -> File f :> Entry
    | _                  -> Entry e
  
  let path (Entry e) = e.Path
  let name (Entry e) = e.Name
  let isDir (Entry e) = e.IsDirectory
  let isFile (Entry e) = e.IsFile

[<RequireQualifiedAccess>]
module File =
  let inline internal (|File|) (f: File) = f.File

  let read (File f) = f.Read ()
  let ofEntry (e: Entry) =
    match e with
    | :? File as f -> f
    | _ -> failwithf "Not a file"

[<RequireQualifiedAccess>]
module Dir =
  let inline internal (|Dir|) (d: Dir) = d.Dir

  let entries (Dir d) = d.Entries |> IOSeq.map Entry.ofEntry

  let dirs (Dir d) = d.Dirs |> IOSeq.map Dir

  let files (Dir d) = d.Files |> IOSeq.map File


[<RequireQualifiedAccess>]
module Fs =

  let getDir path = IO <| fun sys cont ->
    IO.fork (sys.GetDir path |> IO.map Dir) sys cont
