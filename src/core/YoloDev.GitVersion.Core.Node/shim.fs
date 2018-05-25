module internal rec YoloDev.GitVersion.Core.Node.Shim

open System
open YoloDev.GitVersion.Core.Abstractions
open YoloDev.GitVersion.SystemBuilders
open YoloDev.GitVersion.Core.Git
open Bindings
open Fable.Core
open Fable.Import
open Fable.PowerPack
open Fable
open Bindings.NodeGit
open System.Security.Cryptography

[<AutoOpen>]
module internal Helpers =

  let inline dispose (d: #IDisposable) = d.Dispose ()

  let nullArg (name: string) = failwithf "Argument %s cannot be null" name

  let (|OCommit|OTag|) (o: NodeGit.GitObject) =
    match o.``type``() with
    | NodeGit.ObjectType.COMMIT -> OCommit
    | NodeGit.ObjectType.TAG    -> OTag
    | t                         -> failwithf "Type %O not supported" t

  [<Emit("debugger;")>]
  let debugger () : unit = jsNative

  let inline debuggerF f v = debugger (); f v

  [<RequireQualifiedAccess>]
  module Promise =
    let unit x = Promise.lift x

    let defer<'t> () =
      let mutable resolve = Unchecked.defaultof<_>
      let mutable reject = Unchecked.defaultof<_>
      let promise = Promise.create (fun (res: 't -> unit) rej ->
        resolve <- res
        reject <- rej
        ())

      promise, resolve, reject

  [<RequireQualifiedAccess>]
  module IO =
    let ofPromiseFactory f = IO <| fun _ cont ->
      f ()
      |> Promise.eitherEnd (Ok >> cont >> ignore) (Error >> cont >> ignore)
      FakeUnit
    
    let debug io = IO <| fun sys cont ->
      IO.fork io sys <| fun r ->
        debugger ()
        cont r
  
  // Note, this code get's compiled to JS, so this completely
  // ignores thread safety
  type LazyGitRef<'t>(io: IO<'t option>, free: 't -> unit) =
    let mutable disposed = false
    let mutable value = None
    let io = 
      io
      |> IO.tap (fun v ->
        if disposed
        then match v with | None -> () | Some v -> free v
        else value <- Some v)
      |> IO.cache

    member __.Value =
      if disposed
      then failwithf "Ref already disposed"
      else io
    
    interface IDisposable with
      member __.Dispose () =
        if not disposed
        then
          match value with
          | None -> ()
          | Some v -> 
            match v with 
            | None -> () 
            | Some v -> free v
  
  module LazyGitRef =
    let create (repo: Repository) (id: ObjectId) (factory: NodeGit.Repository -> NodeGit.Oid -> JS.Promise<_>) free =
      let io = IO.ofPromiseFactory (fun () -> factory repo.Repo id.Oid)
      new LazyGitRef<_> (io, free)
    
    let ofReference<'t when 't :> GitObject> (repo: Repository) (reference: Reference) (ty: System.Type) =
      let io =
        reference.DirectReference
        |> IO.bind (function | None -> IO.unit None | Some d -> d.Target)
        |> IO.bind (function | None -> IO.unit None | Some t -> repo.Lookup<'t> (t.Id, ty))
      new LazyGitRef<'t> (io, dispose)
    
    let get (r: LazyGitRef<_>) = r.Value
  
  [<GeneralizableValue>]
  let safeLookup factory repo oid =
    factory repo oid
    |> Promise.map Some
    |> Promise.catch (fun _ -> None)

[<Literal>]
let HEX_SIZE = 40

let n = NodeGit.nodegit

type ObjectType =
  | Commit
  | Tree
  | Blob
  | Tag

module ObjectType =
  
  let toInt o =
    match o with
    | Commit -> 1
    | Tree   -> 2
    | Blob   -> 3
    | Tag    -> 4
  
  let toNodeGit o =
    match o with
    | Commit -> NodeGit.ObjectType.COMMIT
    | Tree   -> NodeGit.ObjectType.TREE
    | Blob   -> NodeGit.ObjectType.BLOB
    | Tag    -> NodeGit.ObjectType.TAG
  
  let ofInt i =
    match i with
    | 1 -> ObjectType.Commit
    | 2 -> ObjectType.Tree
    | 3 -> ObjectType.Blob
    | 4 -> ObjectType.Tag
    | _ -> invalidArg "i" "Number must be a valid object type"
  
  let ofType t =
    if   t = typeof<Commit> then ObjectType.Commit
    elif t = typeof<Tag> then ObjectType.Tag
    else failwith "Not implemented"

/// <summary>
/// A Repository is the primary interface into a git repository
/// </summary>
type IRepository =
  inherit IDisposable

  abstract Lookup: ObjectId -> IO<GitObject option>
  abstract Lookup: string -> IO<GitObject option>
  abstract Lookup: ObjectId * ObjectType -> IO<GitObject option>
  abstract Lookup: string * ObjectType -> IO<GitObject option>

[<AutoOpen>]
module RepositoryExtensions =
  let inline private allowOrphanReference (repo: Repository) (identifier: string) =
    if identifier = "HEAD"
    then IO.unit true
    else 
      repo.Head
      |> IO.map (fun (head: Branch) -> head.CanonicalName = identifier)

  type Repository with
    member r.Lookup<'t when 't :> GitObject> (objectish: string, t: System.Type) : IO<'t option> =
      (r :> IRepository).Lookup (objectish, ObjectType.ofType t)
      |> IO.map (Option.map (fun o -> o :?> 't))

    member r.Lookup<'t when 't :> GitObject> (id: ObjectId, t: System.Type) : IO<'t option> =
      (r :> IRepository).Lookup (id, ObjectType.ofType t)
      |> IO.map (Option.map (fun o -> o :?> 't))

    member r.DereferenceToCommit (identifier: string) =
      allowOrphanReference r identifier
      |> IO.bind (fun allowOrphan ->
        IO.ofPromiseFactory (fun () -> 
          promise {
            try
              debugger ()
              let! obj = NodeGit.nodegit.Object.lookupS r.Repo identifier NodeGit.ObjectType.ANY
              debugger ()
              let! commit = obj.peel NodeGit.ObjectType.COMMIT
              debugger ()
              if isNull commit
              then return None
              else return commit.id () |> ObjectId |> Some
            with | _ -> return None
          }) 
        |> IO.tap (debuggerF (function 
                                | None when not allowOrphan -> failwithf "Commit with identifier '%s' not found" identifier 
                                | _ -> ())))

    
    member r.SingleCommittish (identifier: obj) =
      match identifier with
      | null                   -> IO.unit None
      | :? string as s         -> r.DereferenceToCommit s
      | :? ObjectId as id      -> r.DereferenceToCommit id.Sha
      | :? Commit as c         -> IO.unit (Some c.Id)
      | :? TagAnnotation as ta -> ta.Target |> IO.bind (fun (t: GitObject) -> r.DereferenceToCommit t.Id.Sha)
      | :? Tag as ta           -> ta.Target |> IO.bind (fun (t: GitObject) -> r.DereferenceToCommit t.Id.Sha)
      | :? Reference as ref    -> r.DereferenceToCommit ref.CanonicalName
      | :? Branch as b         ->
        b.Tip
        |> IO.bind (fun tip ->
          b.IsCurrentRepositoryHead
          |> IO.bind (fun isCurrentRepoHead ->
            if Option.isSome tip && not isCurrentRepoHead
            then IO.unit (Some tip.Value.Id)
            else IO.unit None))
      | _ -> IO.unit None

/// <summary>
/// Can be used to reference the <see cref="IRepository" /> from which
/// an instance was created.
/// <para>
/// While convenient in some situations (e.g. Checkout branch bound to UI element),
/// it is important to ensure instances created from an <see cref="IRepository" />
/// are not used after it is disposed.
/// </para>
/// <para>
/// It's generally better to create <see cref="IRepository" /> and dependant instances
/// on demand, with a short lifespan.
/// </para>
/// </summary>
type IBelongToARepository =

  /// <summary>
  /// The <see cref="IRepository" /> from which this instance was created.
  /// <para>
  /// The returned value should not be disposed.
  /// </para>
  /// </summary>
  abstract Repository: IRepository

/// <summary>
/// Uniquely identifies a <see cref="GitObject"/>.
/// </summary>
[<Sealed>]
type ObjectId internal (oid: NodeGit.Oid) =
  let oid = 
    match oid with
    | null -> nullArg "oid"
    | o when o.iszero () = 1 -> invalidArg "oid" "object id cannot be zero"
    | _    -> oid
  
  new (str: string) = ObjectId (n.Oid.fromString str)

  member internal __.Oid = oid

  /// <summary>
  /// Gets the sha.
  /// </summary>
  member __.Sha = oid.tostrS ()

  override o.Equals obj =
    match obj with
    | :? ObjectId as other -> (o :> IEquatable<ObjectId>).Equals other
    | _ -> false
  

  override o.GetHashCode () =
    hash o.Sha
  
  interface IEquatable<ObjectId> with

    member o.Equals other = o.Sha = other.Sha
  
  /// <summary>
  /// Returns the <see cref="Sha"/>, a <see cref="String"/> representation of the current <see cref="ObjectId"/>.
  /// </summary>
  /// <returns>The <see cref="Sha"/> that represents the current <see cref="ObjectId"/>.</returns>
  override o.ToString () = o.Sha

  /// <summary>
  /// Returns the <see cref="Sha"/>, a <see cref="String"/> representation of the current <see cref="ObjectId"/>.
  /// </summary>
  /// <param name="prefixLength">The number of chars the <see cref="Sha"/> should be truncated to.</param>
  /// <returns>The <see cref="Sha"/> that represents the current <see cref="ObjectId"/>.</returns>
  member o.ToString (prefixLength: int) =
    let normalizedLength =
      match prefixLength with
      | n when n < 1       -> 1
      | n when n > HEX_SIZE -> HEX_SIZE
      | n                  -> n
    
    let sha = o.Sha
    sha.Substring (0, Math.Min (sha.Length, normalizedLength))

[<AbstractClass>]
type GitObject internal (repo: Repository, oid: ObjectId) =

  /// <summary>
  /// Gets the id of this object
  /// </summary>
  abstract member Id: ObjectId

  /// <summary>
  /// Gets the 40 character sha1 of this object.
  /// </summary>
  abstract member Sha: string

  // defaults
  default o.Id = oid
  default o.Sha = oid.Sha

  override o.Equals obj =
    match obj with
    | :? GitObject as obj -> (o :> IEquatable<GitObject>).Equals obj
    | _                 -> false
  
  override o.GetHashCode () =
    oid.GetHashCode () + (hash "GitObject")
  
  override o.ToString () =
    oid.ToString ()
  
  [<PassGenerics>]
  member o.Peel<'t when 't :> GitObject> (t: System.Type) =
    let otype = ObjectType.ofType t |> ObjectType.toNodeGit
    IO.ofPromiseFactory (fun () ->
      NodeGit.nodegit.Object.lookup repo.Repo oid.Oid NodeGit.ObjectType.ANY
      |> Promise.bind (fun o -> 
        let p = o.peel otype
        o.free ()
        p)
      |> Promise.map (fun o ->
        let oid = ObjectId (o.id ())
        o.free ()
        oid))
    |> IO.bind (fun id -> repo.Lookup<'t> (id, t))
    |> IO.map Option.get
  
  interface IEquatable<GitObject> with
    member o.Equals other =
      (o :> IBelongToARepository).Repository = (other :> IBelongToARepository).Repository
      && o.Id = other.Id
  
  interface IBelongToARepository with
    member o.Repository = repo :> IRepository
  
  abstract Dispose: unit -> unit

  interface IDisposable with
    member o.Dispose () = o.Dispose ()

[<Sealed>]
type Repository internal (repo: NodeGit.Repository) as this =

  let refs = ReferenceCollection this
  let tags = TagCollection this
  let commits = QueryableCommitLog this

  let toImpl (repo: Repository) (oid: ObjectId option) = 
    Promise.map <| fun (o: NodeGit.GitObject) ->
      let oid =
        match oid with
        | Some oid -> oid
        | None     -> ObjectId (o.id ())

      match o with
      | null -> None
      | OCommit -> new Commit (repo, oid) :> GitObject |> Some
      | OTag    -> new TagAnnotation (repo, oid) :> GitObject |> Some
      //| _    -> failwith "Not implemented"

  member __.Refs = refs
  member __.Tags = tags
  member __.Commits = commits
  member repo.Head =
    repo.Refs.Head
    |> IO.map (function
                | None     -> failwithf "Corrupt repository. The 'HEAD' reference is missing."
                | Some (ref: Reference) ->
                  match ref with
                  | :? SymbolicReference -> new Branch (repo, ref)
                  | _                    -> new DetachedHead (repo, ref) :> Branch)

  member __.Commit (message: string, author: Signature, committer: Signature) =
    IO.ofPromiseFactory (fun () ->
      repo.createCommitOnHead [||] (Signature.toGit author) (Signature.toGit committer) message)
    |> IO.bind (ObjectId >> commits.Get)
    |> IO.map Option.get
  
  member r.ApplyTag (name: string) =
    r.Head
    |> IO.bind (fun h -> h.Tip)
    |> IO.map (function | None -> failwith "Head has no commits" | Some t -> t)
    |> IO.bind (fun c ->
      IO.ofPromiseFactory (fun () -> repo.createLightweightTag c.Id.Oid name))
    |> IO.bind (fun ref ->
      r.Tags.Get (ref.name ())
      |> IO.tryFinally (fun () -> ref.free ()))
    |> IO.map Option.get

  member internal __.Repo = repo

  interface IDisposable with
    member __.Dispose () = repo.free ()
  
  interface IRepository with
    member r.Lookup (oid: ObjectId) =
      IO.ofPromiseFactory <| fun () ->
        n.Object.lookup repo oid.Oid NodeGit.ObjectType.ANY
        |> toImpl r (Some oid)
    
    member r.Lookup (objectish: string) =
      IO.ofPromiseFactory <| fun () ->
        n.Object.lookupS repo objectish NodeGit.ObjectType.ANY
        |> toImpl r None
    
    member r.Lookup (oid: ObjectId, gitType: ObjectType) =
      IO.ofPromiseFactory <| fun () ->
        n.Object.lookup repo oid.Oid (ObjectType.toNodeGit gitType)
        |> toImpl r (Some oid)
    
    member r.Lookup (objectish: string, gitType: ObjectType) =
      IO.ofPromiseFactory <| fun () ->
        n.Object.lookupS repo objectish (ObjectType.toNodeGit gitType)
        |> toImpl r None

[<AbstractClass>]
type LazyGitObject<'t> internal (repo: Repository, id: ObjectId, factory: NodeGit.Repository -> NodeGit.Oid -> JS.Promise<'t option>, free: 't -> unit) =
  inherit GitObject (repo, id)

  let lazyInst = LazyGitRef.create repo id factory free

  member internal x.InstOpt = LazyGitRef.get lazyInst
  member internal x.Inst = x.InstOpt |> IO.map Option.get
  
  override __.Dispose () = dispose lazyInst

type Commit internal (repo: Repository, id: ObjectId) =
  inherit LazyGitObject<NodeGit.Commit> (repo, id, safeLookup NodeGit.nodegit.Commit.lookup, (fun c -> c.free ()))

  member x.Message =
    x.Inst
    |> IO.map (fun c -> c.message ())

type TagAnnotation internal (repo: Repository, id: ObjectId) =
  inherit LazyGitObject<NodeGit.Tag> (repo, id, safeLookup NodeGit.nodegit.Tag.lookup, (fun t -> t.free ()))

  member x.Name =
    x.Inst
    |> IO.map (fun t -> t.name ())
  
  member x.Target =
    x.Inst
    |> IO.bind (fun t ->
      let oid = ObjectId (t.targetId ())
      (repo :> IRepository).Lookup oid)
    |> IO.map Option.get

[<AbstractClass>]
type Reference internal (repo: Repository, canonicalName: string, targetIdentifier: string) =

  abstract DirectReference: IO<DirectReference option>

  abstract TargetIdentifier: string
  default __.TargetIdentifier = targetIdentifier

  member __.CanonicalName = canonicalName

  override __.ToString () = sprintf "%s => \"%s\"" canonicalName targetIdentifier

  override o.Equals obj =
    match obj with
    | :? Reference as obj -> (o :> IEquatable<Reference>).Equals obj
    | _                   -> false
  
  override o.GetHashCode () =
    (hash repo) + (hash canonicalName) + (hash targetIdentifier)
  
  interface IEquatable<Reference> with
    member o.Equals other =
      (o :> IBelongToARepository).Repository = (other :> IBelongToARepository).Repository
      && string o = string other

  interface IBelongToARepository with
    member __.Repository = repo :> IRepository

[<RequireQualifiedAccess>]
module Reference =
  
  [<Literal>]
  let localBranchPrefix = "refs/heads/"

  [<Literal>]
  let remoteTrackingBranchPrefix = "refs/remotes/"
  
  [<Literal>]
  let tagPrefix = "refs/tags/"

  [<Literal>]
  let notePrefix = "refs/notes/"

  let (|LocalLikeBranchName|_|) (str: string) =
    if str.StartsWith localBranchPrefix
    then LocalLikeBranchName (str.Substring localBranchPrefix.Length) |> Some
    else None
  
  let (|RemoteTrackingLikeBranchName|_|) (str: string) =
    if str.StartsWith remoteTrackingBranchPrefix
    then RemoteTrackingLikeBranchName (str.Substring remoteTrackingBranchPrefix.Length) |> Some
    else None
  
  let (|TagLikeRefName|_|) (str: string) =
    if str.StartsWith tagPrefix
    then TagLikeRefName (str.Substring tagPrefix.Length) |> Some
    else None
  
  let ofReference (repo: Repository) (ref: NodeGit.Reference) =
    let refType = ref.``type`` ()
    let name = ref.name ()

    match refType with
    | NodeGit.ReferenceType.SYMBOLIC ->
      let targetIdentifier = ref.symbolicTarget ()
      repo.Refs.Get targetIdentifier
      |> IO.map (fun targetRef -> new SymbolicReference (repo, name, targetIdentifier, targetRef) :> Reference)
    
    | NodeGit.ReferenceType.OID ->
      let targetOid = ref.target ()

      new DirectReference (repo, name, ObjectId targetOid) :> Reference
      |> IO.unit
    
    | _ -> failwithf "Unable to build a new reference from a type '%A'." refType

type DirectReference internal (repo: Repository, canonicalName: string, targetId: ObjectId) =
  inherit Reference (repo, canonicalName, targetId.Sha)

  override r.DirectReference = IO.unit (Some r)

  member __.Id = targetId

  member __.Target: IO<GitObject option> =
    (repo :> IRepository).Lookup targetId

type SymbolicReference internal (repo: Repository, canonicalName: string, targetIdentifier: string, target: Reference option) =
  inherit Reference (repo, canonicalName, targetIdentifier)

  override r.DirectReference =
    match target with
    | None -> IO.unit None
    | Some t -> t.DirectReference

[<AbstractClass>]
type ReferenceWrapper<'o when 'o :> GitObject> internal (repo: Repository, reference: Reference, canonicalNameSelector: Reference -> string, t: System.Type) =
  let canonicalName = canonicalNameSelector reference
  let lazyInst = LazyGitRef.ofReference<'o> repo reference t

  abstract CanonicalName: string
  default __.CanonicalName = canonicalName

  abstract FriendlyName: string
  default r.FriendlyName = r.Shorten ()

  abstract Shorten: unit -> string

  abstract Reference: Reference
  default __.Reference = reference

  member internal __.TargetObject = LazyGitRef.get lazyInst

type Tag internal (repo: Repository, reference: Reference, canonicalName: string) =
  inherit ReferenceWrapper<GitObject> (repo, reference, (fun _ -> canonicalName), typeof<GitObject>)

  member tag.Annotation =
    tag.TargetObject
    |> IO.map (function | Some (:? TagAnnotation as ta) -> Some ta | _ -> None)

  abstract Target: IO<GitObject>
  default tag.Target =
    tag.TargetObject
    |> IO.bind (function | Some (:? TagAnnotation as ta) -> ta.Target | o -> IO.unit (Option.get o))
  
  override tag.Shorten () =
    tag.CanonicalName.Substring Reference.tagPrefix.Length

type Branch internal (repo: Repository, reference: Reference, canonicalNameSelector: Reference -> string) =
  inherit ReferenceWrapper<Commit> (repo, reference, canonicalNameSelector, typeof<Commit>)

  new (repo: Repository, reference: Reference, canonicalName: string) = 
    Branch (repo, reference, fun _ -> canonicalName)
  new (repo: Repository, reference: Reference) = 
    Branch (repo, reference, fun r -> r.TargetIdentifier)

  abstract Tip: IO<Commit option>
  default branch.Tip = branch.TargetObject

  abstract IsCurrentRepositoryHead: IO<bool>
  default branch.IsCurrentRepositoryHead =
    repo.Head
    |> IO.map (fun head ->
      match branch with
      | :? DetachedHead -> head.Reference.TargetIdentifier = branch.Reference.TargetIdentifier
      | _               -> head.Reference.TargetIdentifier = branch.CanonicalName)

  override branch.Shorten () =
    match branch.CanonicalName with
    | Reference.LocalLikeBranchName          n -> n
    | Reference.RemoteTrackingLikeBranchName n -> n
    | n -> invalidArg "" (sprintf "'%s' does not look like a valid branch name." n)

type DetachedHead internal (repo: Repository, reference: Reference) =
  inherit Branch (repo, reference, fun _ -> "(no branch)")

  override branch.Shorten () = branch.CanonicalName

type QueryableCommitLog internal (repo: Repository, filter: ICommitFilter) =
  new (repo: Repository) = QueryableCommitLog (repo, CommitFilter.revWalk)

  member __.QueryBy filter = QueryableCommitLog (repo, filter)

  member __.Get (id: ObjectId) = repo.Lookup<Commit> (id, typeof<Commit>)

  member internal __.ToSeq () =
    IOSeq.delay <| fun () ->
      debugger ()
      let revwalk = NodeGit.nodegit.Revwalk.create repo.Repo

      let pushIO =
        filter.IncludeReachableFrom
        |> IOSeq.ofSeq
        |> IOSeq.chooseM (debuggerF repo.SingleCommittish)
        |> IO.map (Seq.iter (fun oid -> debugger (); revwalk.push oid.Oid))
      
      let hideIO =
        filter.ExcludeReachableFrom
        |> IOSeq.ofSeq
        |> IOSeq.chooseM repo.SingleCommittish
        |> IO.map (Seq.iter (fun oid -> debugger (); revwalk.push oid.Oid))
      
      let prepareIO =
        IO.combine pushIO hideIO
        |> IO.tap (fun () -> debugger (); if filter.FirstParentOnly then revwalk.simplifyFirstParent ())

      // Important: this IO is not cached, as such, every time it get's called it will progress
      // the revwalk, which makes the `takeWhile` bellow work.
      let next =
        IO.ofPromiseFactory (debuggerF revwalk.next)
        |> IO.map Option.ofObj
        |> IO.bind (function | None -> IO.unit None | Some oid -> repo.Lookup<Commit> (ObjectId oid, typeof<Commit>))
        |> IOSeq.ofIO
      
      // TODO: This fails when no commits are present. Probably needs special case :-/
      IOSeq.takeWhile Option.isSome next
      |> IOSeq.map Option.get
      |> IOSeq.doFirst prepareIO
      |> IOSeq.tryFinally revwalk.free
    (* ioSeq {
      let revwalk = NodeGit.nodegit.Revwalk.create repo.Repo
      let next = IO.ofPromiseFactory revwalk.next
      try
        let mutable isDone = false
        while not isDone do
          let! oid = next
          if isNull oid
          then isDone <- true
          else
            let! commit = repo.Lookup<Commit> (ObjectId oid)
            yield Option.get commit
      finally
        revwalk.free ()
    } *)

type ReferenceCollection internal (repo: Repository) =

  member refs.Get name : IO<Reference option> = refs.Resolve name

  member refs.Head = refs.Get "HEAD"

  member internal __.Resolve (name: string) =
    if isNull name then nullArg "name"
    elif String.IsNullOrEmpty name then invalidArg "name" "name cannot be empty"
    else
      IO.ofPromiseFactory (fun () -> NodeGit.nodegit.Reference.lookupS repo.Repo name)
      |> IO.toResult
      |> IO.map (function | Ok r -> Some r | Error _ -> None)
      |> IO.bind (function | None -> IO.unit None | Some r -> Reference.ofReference repo r |> IO.map Some)

type TagCollection internal (repo: Repository) =

  member __.Get (name: string) : IO<Tag option> =
    let refName =
      match name with
      | null                          -> nullArg "name"
      | s when String.IsNullOrEmpty s -> invalidArg "name" "name cannot be empty"
      | Reference.TagLikeRefName t    -> t
      | s                             -> s
    
    repo.Refs.Resolve refName
    |> IO.map (function | None -> None | Some ref -> new Tag (repo, ref, refName) |> Some)
  
  member tags.Seq (*: IOSeq<Tag>*) =
    IO.ofPromiseFactory (fun () -> NodeGit.nodegit.Tag.list repo.Repo)
    |> IO.map IOSeq.ofSeq
    |> IOSeq.bindIO id
    |> IOSeq.chooseM tags.Get

type Signature = 
  { name: string
    email: string }

module Signature =

  let toGit (s: Signature) =
    NodeGit.nodegit.Signature.now s.name s.email 

type CommitOptions = 
  { amendPreviousCommit: bool
    allowEmptyCommit: bool
    prettifyMessage: bool
    commentaryChar: char option }

module CommitOptions =
  
  let defaultOptions =
    { amendPreviousCommit = false
      allowEmptyCommit = false
      prettifyMessage = true
      commentaryChar = None }