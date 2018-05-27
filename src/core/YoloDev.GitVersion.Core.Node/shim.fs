module internal rec YoloDev.GitVersion.Core.Node.Shim

open System
open YoloDev.GitVersion.Core.Abstractions
open YoloDev.GitVersion.SystemBuilders
open YoloDev.GitVersion.Core.Git
open YoloDev.GitVersion.Core.Logging
open YoloDev.GitVersion.Core.Logging.Message
open Bindings
open Fable.Core
open Fable.Import
open Fable.PowerPack
open Fable
open Bindings.NodeGit
open System.Security.Cryptography

let logger = Log.create "YoloDev.GitVersion.Core.Node.Shim"

[<AutoOpen>]
module internal Helpers =

  let dispose (d: #IDisposable) = d.Dispose ()

  let nullArg (name: string) = failwithf "Argument %s cannot be null" name

  let (|OCommit|OTag|) (o: NodeGit.GitObject) =
    match o.``type``() with
    | NodeGit.ObjectType.COMMIT -> OCommit
    | NodeGit.ObjectType.TAG    -> OTag
    | t                         -> failwithf "Type %O not supported" t

  [<Emit("debugger;")>]
  let debugger () : unit = jsNative

  [<Emit("(($0 && $0.errno) | 0) || 0")>]
  let __errorCode (_e: #exn) : int = jsNative

  let (|NodeError|_|) (no: NodeGit.ErrorCode) (e: #exn) =
    match __errorCode e with
    | n when n = int no -> Some NodeError
    | _                 -> None

  [<RequireQualifiedAccess>]
  module Promise =
    let unit x = Promise.lift x

    let fail e : JS.Promise<_> = promise { return raise e }

    let defer<'t> () =
      let mutable resolve = Unchecked.defaultof<_>
      let mutable reject = Unchecked.defaultof<_>
      let promise = Promise.create (fun (res: 't -> unit) rej ->
        resolve <- res
        reject <- rej
        ())

      promise, resolve, reject
    
    let safeLookup p = p |> Promise.catchBind (fun e ->
      match e with
      | NodeError NodeGit.ErrorCode.NotFound -> Promise.unit None
      | _                                    -> Promise.fail e)

  [<RequireQualifiedAccess>]
  module IO =
    let ofPromiseFactory f = IO <| fun _ cont ->
      f ()
      |> Promise.eitherEnd (Ok >> cont >> ignore) (Error >> cont >> ignore)
      FakeUnit
  
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

[<RequireQualifiedAccess>]
type LookupOptions =
  | None = 0
  | ThrowWhenNoGitObjectHasBeenFound = 1
  | DereferenceResultToCommit = 2
  | ThrowWhenCanNotBeDereferencedToACommit = 4
  
[<RequireQualifiedAccess>]
module LookupOptions =

  let private hasFlag (f: LookupOptions) (l: LookupOptions) =
    (l &&& f) = f

  let throwWenNoGitObjectHasBeenFound = hasFlag LookupOptions.ThrowWhenNoGitObjectHasBeenFound
  let dereferenceResultToCommit = hasFlag LookupOptions.DereferenceResultToCommit
  let throwWhenCanNotBeDereferencedToACommit = hasFlag LookupOptions.ThrowWhenCanNotBeDereferencedToACommit

type ObjectType =
  | Any
  | Commit
  | Tree
  | Blob
  | Tag

[<RequireQualifiedAccess>]

module ObjectType =
  
  let toInt o =
    match o with
    | Any    -> -2
    | Commit -> 1
    | Tree   -> 2
    | Blob   -> 3
    | Tag    -> 4
  
  let toNodeGit o =
    match o with
    | Any    -> NodeGit.ObjectType.ANY
    | Commit -> NodeGit.ObjectType.COMMIT
    | Tree   -> NodeGit.ObjectType.TREE
    | Blob   -> NodeGit.ObjectType.BLOB
    | Tag    -> NodeGit.ObjectType.TAG
  
  let ofInt i =
    match i with
    | -2 -> ObjectType.Any 
    | 1 -> ObjectType.Commit
    | 2 -> ObjectType.Tree
    | 3 -> ObjectType.Blob
    | 4 -> ObjectType.Tag
    | _ -> invalidArg "i" "Number must be a valid object type"
  
  let ofType t =
    if   t = typeof<Commit> then ObjectType.Commit
    elif t = typeof<Tag> then ObjectType.Tag
    elif t = typeof<GitObject> then ObjectType.Any
    else debugger (); failwith "Not implemented"

[<AutoOpen>]
module RepositoryExtensions =
  let private allowOrphanReference (repo: Repository) (identifier: string) =
    if identifier = "HEAD"
    then IO.unit true
    else 
      repo.Head
      |> IO.map (fun (head: Branch) -> head.CanonicalName = identifier)

  type Repository with
    member r.Lookup<'t when 't :> GitObject> (objectish: string, t: System.Type) : IO<'t option> =
      r.Lookup (objectish, ObjectType.ofType t)
      |> IO.map (Option.map (fun (o: GitObject) -> o :?> 't))

    member r.Lookup<'t when 't :> GitObject> (id: ObjectId, t: System.Type) : IO<'t option> =
      r.Lookup (id, ObjectType.ofType t)
      |> IO.map (Option.map (fun (o: GitObject) -> o :?> 't))

    member r.DereferenceToCommit (identifier: string) =
      allowOrphanReference r identifier
      |> IO.bind (fun allowOrphan ->
        let options =
          if not allowOrphan
          then LookupOptions.DereferenceResultToCommit ||| LookupOptions.ThrowWhenNoGitObjectHasBeenFound
          else LookupOptions.DereferenceResultToCommit
        r.Lookup (identifier, ObjectType.Any, options))
      |> IO.map (Option.map (fun o -> o.Id))
    
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
  abstract Repository: Repository

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
        match o with
        | None -> None
        | Some o ->
          let oid = ObjectId (o.id ())
          o.free ()
          Some oid))
    |> IO.bind (function | None -> IO.unit None | Some id -> repo.Lookup<'t> (id, t))
  
  interface IEquatable<GitObject> with
    member o.Equals other =
      (o :> IBelongToARepository).Repository = (other :> IBelongToARepository).Repository
      && o.Id = other.Id
  
  interface IBelongToARepository with
    member o.Repository = repo
  
  abstract Dispose: unit -> unit

  interface IDisposable with
    member o.Dispose () = o.Dispose ()

[<Sealed>]
type Repository internal (repo: NodeGit.Repository) as this =

  let refs = ReferenceCollection this
  let tags = TagCollection this
  let commits = QueryableCommitLog this
  let index = Index this

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
    io {
      let! head = r.Head

      let! tip = head.Tip

      match tip with
      | None -> return failwith "Head has no commits"
      | Some commit ->
        do! logger.debugIO (
              eventX "Tag {commit} with {name}"
              >> setField "commit" commit.Sha
              >> setField "name" name)
        let! ref = IO.ofPromiseFactory (fun () -> repo.createLightweightTag commit.Id.Oid name)

        let name = ref.name ()
        
        let! tag = r.Tags.Get name
        return Option.get tag
    }

  member internal __.Repo = repo
  
  member r.Lookup (oid: ObjectId): IO<GitObject option> =
    r.Lookup (oid, ObjectType.Any)
  
  member r.Lookup (objectish: string): IO<GitObject option> =
    r.Lookup (objectish, ObjectType.Any)
  
  member r.Lookup (oid: ObjectId, gitType: ObjectType): IO<GitObject option> =
    IO.ofPromiseFactory <| fun () ->
      n.Object.lookup repo oid.Oid (ObjectType.toNodeGit gitType)
      |> toImpl r (Some oid)
  
  member r.Lookup (objectish: string, gitType: ObjectType): IO<GitObject option> =
    r.Lookup (objectish, gitType, LookupOptions.None)

  // TODO: Add LookUpOptions
  member r.Lookup (objectish: string, gitType: ObjectType, options: LookupOptions): IO<GitObject option> =
    IO.ofPromiseFactory <| fun () ->
      promise {
        let! objOpt = NodeGit.nodegit.Revparse.single r.Repo objectish |> Promise.safeLookup
          
        match objOpt with
        | None ->
          if LookupOptions.throwWenNoGitObjectHasBeenFound options
          then return failwithf "No git object was found with name '%s'" objectish
          else return None
        | Some obj ->
          let objType = obj.``type`` ()
          if gitType <> ObjectType.Any && ObjectType.toNodeGit gitType <> objType
          then
            if LookupOptions.throwWenNoGitObjectHasBeenFound options
            then return failwithf "No git object was found with name '%s'" objectish
            else return None
          else
            let! objOpt =
              if LookupOptions.dereferenceResultToCommit options
              then obj.peel NodeGit.ObjectType.COMMIT |> Promise.safeLookup
              else Promise.unit (Some obj)
            
            match objOpt with
            | None ->
              if LookupOptions.throwWhenCanNotBeDereferencedToACommit options
              then return failwithf "Could not dereference to commit: '%s'" objectish
              else return None
            | Some o -> return! toImpl r None (Promise.unit o)
      }
    
    member internal __.ReloadFromDisk () =
      index.Read ()
    
    member r.RetrieveStatus (options: StatusOptions option) =
      io {
        do! r.ReloadFromDisk ()

        return RepositoryStatus (r, options)
      }
    
  
  interface IDisposable with
    member __.Dispose () = repo.free ()

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
      repo.Lookup oid)
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
    member __.Repository = repo

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
    repo.Lookup targetId

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
    ioSeq {
      let revwalk = NodeGit.nodegit.Revwalk.create repo.Repo
      try
        for inc in filter.IncludeReachableFrom do
          let! optId = repo.SingleCommittish inc
          match optId with
          | None -> ()
          | Some id -> 
            revwalk.push id.Oid
        
        for inc in filter.ExcludeReachableFrom do
          let! optId = repo.SingleCommittish inc
          match optId with
          | None -> ()
          | Some id -> 
            revwalk.hide id.Oid
        
        if filter.FirstParentOnly then
          revwalk.simplifyFirstParent ()
        
        try
          let mutable atEnd = false
          while not atEnd do
            let! oid = IO.ofPromiseFactory revwalk.next

            if isNull oid 
            then atEnd <- true
            else
              let! commit = repo.Lookup<Commit> (ObjectId oid, typeof<Commit>)
              yield Option.get commit
        with
        | NodeError NodeGit.ErrorCode.IterOver -> ()
        | e                                    -> raise e
      finally
        revwalk.free ()
    }

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
    
    let canonicalName = Reference.tagPrefix + refName 
    repo.Refs.Resolve canonicalName
    |> IO.map (function | None -> None | Some ref -> new Tag (repo, ref, canonicalName) |> Some)
  
  member tags.Seq =
    ioSeq {
      let! tagNames = IO.ofPromiseFactory (fun () -> NodeGit.nodegit.Tag.list repo.Repo)

      for tagName in tagNames do
        let! tag = tags.Get tagName
        yield Option.get tag
    }

type Index internal (repo: Repository) =
  let _ref = new LazyGitRef<NodeGit.Index> (IO.ofPromiseFactory repo.Repo.index |> IO.map Some, ignore)

  member private __.Ref =
    LazyGitRef.get _ref
    |> IO.map Option.get

  member i.Read () =
    i.Ref
    |> IO.bind (fun i -> IO.ofPromiseFactory (fun () -> i.read true))
    |> IO.map ignore

type RepositoryStatus internal (repo: Repository, options: StatusOptions option) =

  let createStatusOptions (options: StatusOptions) =
    let coreOptions = NodeGit.nodegit.StatusOptions.create ()
    coreOptions.version <- 1
    coreOptions.show <- enum (int options.show)

    let addFlag b f =
      if b then coreOptions.flags <- coreOptions.flags ||| f

    addFlag options.includeIgnored GitStatusOptionFlags.IncludeIgnored
    addFlag options.includeUntracked GitStatusOptionFlags.IncludeUntracked
    addFlag options.detectRenamesInIndex (GitStatusOptionFlags.RenamesHeadToIndex ||| GitStatusOptionFlags.RenamesFromRewrites)
    addFlag options.detectRenamesInWorkDir (GitStatusOptionFlags.RenamesIndexToWorkDir ||| GitStatusOptionFlags.RenamesFromRewrites)
    addFlag options.excludeSubmodules GitStatusOptionFlags.ExcludeSubmodules
    addFlag options.recurseIgnoredDirs GitStatusOptionFlags.RecurseIgnoredDirs
    addFlag options.recurseUntrackedDirs GitStatusOptionFlags.RecurseUntrackedDirs
    addFlag options.disablePathSpecMatch GitStatusOptionFlags.DisablePathspecMatch
    addFlag options.includeUnaltered GitStatusOptionFlags.IncludeUnmodified

    match options.pathSpec with
    | None   -> ()
    | Some l -> coreOptions.pathspec <- Array.ofList l

    coreOptions
  
  let statusEntryForDelta (gitStatus: FileStatus) (deltaHeadToIndex: DiffDelta option) (deltaIndexToWorkDir: DiffDelta option) =
    let headToIndexRenameDetails =
      match deltaHeadToIndex with
      | Some delta when FileStatus.has FileStatus.RenamedInIndex gitStatus ->
        Some <| RenameDetails.ofDelta delta
      | _ -> None
    let indexToWorkDirRenameDetails =
      match deltaIndexToWorkDir with
      | Some delta when FileStatus.has FileStatus.RenamedInWorkdir gitStatus ->
        Some <| RenameDetails.ofDelta delta
      | _ -> None

    let filePath =
      deltaIndexToWorkDir
      |> Option.map (fun delta -> delta.newFile.path ())
      |> Option.orElseWith (fun () -> 
        deltaHeadToIndex
        |> Option.map (fun delta -> delta.newFile.path ()))
      |> Option.defaultWith (fun () -> failwithf "Neither of the deltas are avail")
    
    { StatusEntry.filePath = filePath
      state = gitStatus
      headToIndexRenameDetails = headToIndexRenameDetails
      indexToWorkDirRenameDetails = indexToWorkDirRenameDetails }
  
  let _entries =
    IO.ofPromiseFactory (fun () ->
      promise {
        let opts = createStatusOptions (options |> Option.defaultValue StatusOptions.defaultOptions)
        let! list = NodeGit.nodegit.StatusList.create repo.Repo opts
        try return List.ofSeq <|
              seq {
                for i in 0 .. list.entrycount () - 1 do
                  let entry = NodeGit.nodegit.Status.byIndex list i
                  yield statusEntryForDelta (enum entry.status) entry.headToIndex entry.indexToWorkdir
              }
        finally
          list.free ()
      })
    |> IO.cache
  
  member __.IsDirty =
    _entries
    |> IO.map (Seq.exists StatusEntry.isDirty)

type RenameDetails =
  { oldFilePath: string
    newFilePath: string
    similarity: int }

module RenameDetails =

  let ofDelta (delta: DiffDelta) =
    { oldFilePath = delta.oldFile.path ()
      newFilePath = delta.newFile.path ()
      similarity = delta.similarity }

type StatusEntry =
  { filePath: string
    state: FileStatus
    headToIndexRenameDetails: RenameDetails option
    indexToWorkDirRenameDetails: RenameDetails option }

module StatusEntry =
  let isDirty entry =
    entry.state <> FileStatus.Ignored && entry.state <> FileStatus.Unaltered 

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

/// <summary>
/// Flags controlling what files are reported by status.
/// </summary>
[<RequireQualifiedAccess>]
type StatusShowOption =
  /// <summary>
  /// Both the index and working directory are examined for changes
  /// </summary>
  | IndexAndWorkDir = 0

  /// <summary>
  /// Only the index is examined for changes
  /// </summary>
  | IndexOnly = 1

  /// <summary>
  /// Only the working directory is examined for changes
  /// </summary>
  | WorkDirOnly = 2

/// <summary>
/// Options controlling the status behavior.
/// </summary>
type StatusOptions =
  { 
    /// <summary>
    /// Which files should be scanned and returned
    /// </summary>
    show: StatusShowOption

    /// <summary>
    /// Examine the staged changes for renames.
    /// </summary>
    detectRenamesInIndex: bool

    /// <summary>
    /// Examine unstaged changes in the working directory for renames.
    /// </summary>
    detectRenamesInWorkDir: bool

    /// <summary>
    /// Exclude submodules from being scanned for status
    /// </summary>
    excludeSubmodules: bool

    /// <summary>
    /// Recurse into ignored directories
    /// </summary>
    recurseIgnoredDirs: bool

    /// <summary>
    /// Recurse into untracked directories
    /// </summary>
    recurseUntrackedDirs: bool

    /// <summary>
    /// Limit the scope of paths to consider to the provided pathspecs
    /// </summary>
    /// <remarks>
    /// If a PathSpec is given, the results from rename detection may
    /// not be accurate.
    /// </remarks>
    pathSpec: string list option

    /// <summary>
    /// When set to <c>true</c>, the PathSpec paths will be considered
    /// as explicit paths, and NOT as pathspecs containing globs.
    /// </summary>
    disablePathSpecMatch: bool

    /// <summary>
    /// Include unaltered files when scanning for status
    /// </summary>
    /// <remarks>
    /// Unaltered meaning the file is identical in the working directory, the index and HEAD.
    /// </remarks>
    includeUnaltered: bool

    /// <summary>
    /// Include ignored files when scanning for status
    /// </summary>
    /// <remarks>
    /// ignored meaning present in .gitignore. Defaults to true for back compat but may improve perf to not include if you have thousands of ignored files.
    /// </remarks>
    includeIgnored: bool

    /// <summary>
    /// Include untracked files when scanning for status
    /// </summary>
    includeUntracked: bool }

module StatusOptions =

  let defaultOptions = 
    { show = StatusShowOption.IndexAndWorkDir
      detectRenamesInIndex = true
      detectRenamesInWorkDir = false
      excludeSubmodules = false
      recurseIgnoredDirs = false
      recurseUntrackedDirs = true
      pathSpec = None
      disablePathSpecMatch = false
      includeUnaltered = false
      includeIgnored = true
      includeUntracked = true }

[<Flags>]
type FileStatus =
  /// <summary>
  /// The file doesn't exist.
  /// </summary>
  | Nonexistent = -2147483648

  /// <summary>
  /// The file hasn't been modified.
  /// </summary>
  | Unaltered = 0 // GIT_STATUS_CURRENT

  /// <summary>
  /// New file has been added to the Index. It's unknown from the Head.
  /// </summary>
  | NewInIndex = 1 // GIT_STATUS_INDEX_NEW

  /// <summary>
  /// New version of a file has been added to the Index. A previous version exists in the Head.
  /// </summary>
  | ModifiedInIndex = 2 // GIT_STATUS_INDEX_MODIFIED

  /// <summary>
  /// The deletion of a file has been promoted from the working directory to the Index. A previous version exists in the Head.
  /// </summary>
  | DeletedFromIndex = 4 // GIT_STATUS_INDEX_DELETED

  /// <summary>
  /// The renaming of a file has been promoted from the working directory to the Index. A previous version exists in the Head.
  /// </summary>
  | RenamedInIndex = 8 // GIT_STATUS_INDEX_RENAMED

  /// <summary>
  /// A change in type for a file has been promoted from the working directory to the Index. A previous version exists in the Head.
  /// </summary>
  | TypeChangeInIndex = 16 // GIT_STATUS_INDEX_TYPECHANGE

  /// <summary>
  /// New file in the working directory, unknown from the Index and the Head.
  /// </summary>
  | NewInWorkdir = 128 // GIT_STATUS_WT_NEW

  /// <summary>
  /// The file has been updated in the working directory. A previous version exists in the Index.
  /// </summary>
  | ModifiedInWorkdir = 256 // GIT_STATUS_WT_MODIFIED

  /// <summary>
  /// The file has been deleted from the working directory. A previous version exists in the Index.
  /// </summary>
  | DeletedFromWorkdir = 512 // GIT_STATUS_WT_DELETED

  /// <summary>
  /// The file type has been changed in the working directory. A previous version exists in the Index.
  /// </summary>
  | TypeChangeInWorkdir = 1024 // GIT_STATUS_WT_TYPECHANGE

  /// <summary>
  /// The file has been renamed in the working directory.  The previous version at the previous name exists in the Index.
  /// </summary>
  | RenamedInWorkdir = 2048 // GIT_STATUS_WT_RENAMED

  /// <summary>
  /// The file is unreadable in the working directory.
  /// </summary>
  | Unreadable = 4096 // GIT_STATUS_WT_UNREADABLE

  /// <summary>
  /// The file is <see cref="NewInWorkdir"/> but its name and/or path matches an exclude pattern in a <c>gitignore</c> file.
  /// </summary>
  | Ignored = 16384 // GIT_STATUS_IGNORED

  /// <summary>
  /// The file is <see cref="Conflicted"/> due to a merge.
  /// </summary>
  | Conflicted = 32768 // GIT_STATUS_CONFLICTED

[<RequireQualifiedAccess>]
module FileStatus =

  let has (f: FileStatus) v =
    (f &&& v) = f
