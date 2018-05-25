module rec Bindings.NodeGit

open System
open Fable.Core
open Fable.Import.JS
open Fable.Core

type ObjectType =
  | ANY = -2
  | BAD = -1
  | EXT1 = 0
  | COMMIT = 1
  | TREE = 2
  | BLOB = 3
  | TAG = 4
  | EXT2 = 5
  | OFS_DELTA = 6
  | REF_DELTA = 7

type ReferenceType =
  | INVALID = 0
  | OID = 1
  | SYMBOLIC = 2
  | LISTALL = 3

[<AllowNullLiteral>]
type Oid =
  abstract tostrS: unit -> string
  abstract iszero: unit -> int

type OidStatic =
  abstract fromString: string -> Oid

[<AllowNullLiteral>]
type GitObject =
  abstract free: unit -> unit
  abstract id: unit -> Oid
  abstract peel: ObjectType -> Promise<GitObject>
  abstract owner: unit -> Repository
  abstract ``type``: unit -> ObjectType

type GitObjectStatic =
  abstract lookup: Repository -> Oid -> ObjectType -> Promise<GitObject>
  [<Emit("$0.lookup($1,$2)")>]
  abstract lookupS: Repository -> string -> ObjectType -> Promise<GitObject>

[<AllowNullLiteral>]
type Reference =
  abstract free: unit -> unit
  abstract isBranch: unit -> int
  abstract isNote: unit -> int
  abstract isRemote: unit -> int
  abstract isTag: unit -> int
  abstract name: unit -> string
  abstract target: unit -> Oid
  abstract peel: ObjectType -> Promise<GitObject>
  abstract owner: unit -> Repository
  abstract symbolicTarget: unit -> string
  abstract ``type``: unit -> ReferenceType

type ReferenceStatic =
  abstract nameToId: Repository -> string -> Promise<Oid>
  abstract lookup: Repository -> Oid -> Promise<Reference>
  [<Emit("$0.lookup($1,$2)")>]
  abstract lookupS: Repository -> string -> Promise<Reference>  

type Tag =
  inherit GitObject
  abstract name: unit -> string
  abstract targetId: unit -> Oid

type TagStatic =
  abstract list: Repository -> Promise<string array>
  abstract lookup: Repository -> Oid -> Promise<Tag>

type Signature =
  abstract free: unit -> unit
  abstract name: string
  abstract email: string
  abstract toString: unit -> string

type BranchStatic =
  abstract name: Reference -> string

type SignatureStatic =
  abstract now: string -> string -> Signature

[<AllowNullLiteral>]
type Commit =
  inherit GitObject
  abstract message: unit -> string
  abstract sha: unit -> string

type CommitStatic =
  abstract lookup: Repository -> Oid -> Promise<Commit>
  [<Emit("$0.lookup($1,$2)")>]
  abstract lookupS: Repository -> string -> Promise<Commit>

type Revwalk =
  abstract free: unit -> unit
  abstract hide: Oid -> unit
  abstract push: Oid -> unit
  abstract simplifyFirstParent: unit -> unit
  abstract next: unit -> Promise<Oid>

type RevwalkStatic =
  abstract create: Repository -> Revwalk

type Repository =
  abstract free: unit -> unit
  abstract head: unit -> Promise<Reference>
  abstract headUnborn: unit -> int
  abstract createLightweightTag: Oid -> string -> Promise<Reference>
  abstract createCommitOnHead: string array -> Signature -> Signature -> string -> Promise<Oid>
  abstract getTagByName: string -> Promise<Tag>

type RepositoryStatic =
  abstract ``open``: string -> Promise<Repository>
  abstract init: string -> int -> Promise<Repository>

type IExports =
  abstract Oid: OidStatic
  abstract Signature: SignatureStatic
  abstract Repository: RepositoryStatic
  abstract Branch: BranchStatic
  abstract Revwalk: RevwalkStatic
  abstract Commit: CommitStatic
  abstract Tag: TagStatic
  abstract Reference: ReferenceStatic
  abstract Object: GitObjectStatic

[<Import("*", "nodegit")>]
let nodegit: IExports = Exceptions.jsNative