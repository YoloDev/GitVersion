module rec Bindings.NodeGit

open System
open Fable.Core
open Fable.Import.JS
open Fable.Core
open System
open System.Security.Cryptography

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

type ErrorCode =
  /// <summary>
  /// Input does not exist in the scope searched.
  /// </summary>
  | NotFound = -3

  /// <summary>
  /// There are no more entries left to iterate.
  /// </summary>
  | IterOver = -31

type GitStatusShow =
  | IndexAndWorkDir = 0
  | IndexOnly = 1
  | WorkDirOnly = 2

[<Flags>]
type GitStatusOptionFlags =
  | IncludeUntracked             = 0b0000000000000001
  | IncludeIgnored               = 0b0000000000000010
  | IncludeUnmodified            = 0b0000000000000100
  | ExcludeSubmodules            = 0b0000000000001000
  | RecurseUntrackedDirs         = 0b0000000000010000
  | DisablePathspecMatch         = 0b0000000000100000
  | RecurseIgnoredDirs           = 0b0000000001000000
  | RenamesHeadToIndex           = 0b0000000010000000
  | RenamesIndexToWorkDir        = 0b0000000100000000
  | SortCaseSensitively          = 0b0000001000000000
  | SortCaseInsensitively        = 0b0000010000000000
  | RenamesFromRewrites          = 0b0000100000000000
  | NoRefresh                    = 0b0001000000000000
  | UpdateIndex                  = 0b0010000000000000
  | IncludeUnreadable            = 0b0100000000000000
  | IncludeUnreadableAsUntracked = 0b1000000000000000

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
  abstract peel: ObjectType -> Promise<GitObject option>
  abstract owner: unit -> Repository
  abstract ``type``: unit -> ObjectType

type GitObjectStatic =
  abstract lookup: Repository -> Oid -> ObjectType -> Promise<GitObject>
  [<Emit("$0.lookup($1,$2,$3)")>]
  abstract lookupS: Repository -> string -> ObjectType -> Promise<GitObject>

type Tree =
  abstract free: unit -> unit

type TreeStatic =
  abstract lookup: Repository -> Oid -> Promise<Tree>

[<AllowNullLiteral>]
type Reference =
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
  abstract isValidName: string -> int

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
  abstract createFromAnnotated: Repository -> string -> AnnotatedCommit -> int -> Promise<Reference option>

type SignatureStatic =
  abstract now: string -> string -> Signature

[<AllowNullLiteral>]
type Commit =
  inherit GitObject
  abstract message: unit -> string
  abstract sha: unit -> string
  abstract treeId: unit -> Oid

type CommitStatic =
  abstract lookup: Repository -> Oid -> Promise<Commit>
  [<Emit("$0.lookup($1,$2)")>]
  abstract lookupS: Repository -> string -> Promise<Commit>

type AnnotatedCommit =
  abstract free: unit -> unit

type AnnotatedCommitStatic =
  abstract lookup: Repository -> Oid -> Promise<AnnotatedCommit option>
  abstract fromRevspec: Repository -> string -> Promise<AnnotatedCommit option>

type Revwalk =
  abstract free: unit -> unit
  abstract hide: Oid -> unit
  abstract push: Oid -> unit
  abstract simplifyFirstParent: unit -> unit
  abstract next: unit -> Promise<Oid>

type RevwalkStatic =
  abstract create: Repository -> Revwalk

type Index =
  abstract read: bool -> Promise<int>

type Repository =
  abstract free: unit -> unit
  abstract head: unit -> Promise<Reference>
  abstract headUnborn: unit -> int
  abstract createLightweightTag: Oid -> string -> Promise<Reference>
  abstract createCommitOnHead: string array -> Signature -> Signature -> string -> Promise<Oid>
  abstract getTagByName: string -> Promise<Tag>
  abstract index: unit -> Promise<Index>
  abstract setHeadDetached: Oid -> int
  abstract setHeadDetachedFromAnnotated: AnnotatedCommit -> int
  abstract setHead: string -> Promise<unit>

type RepositoryStatic =
  abstract ``open``: string -> Promise<Repository>
  abstract init: string -> int -> Promise<Repository>

type RevparseStatic =
  abstract single: Repository -> string -> Promise<GitObject option>

type StatusOptions =
  abstract flags: GitStatusOptionFlags with get,set
  abstract pathspec: string array with get,set
  abstract show: GitStatusShow with get,set
  abstract version: int with get,set

type StatusOptionsStatic =
  [<Emit("new $0()")>]
  abstract create: unit -> StatusOptions

type StatusList =
  abstract entrycount: unit -> int
  abstract free: unit -> unit

type StatusListStatic =
  abstract create: Repository -> StatusOptions -> Promise<StatusList>

type DiffDelta =
  abstract flags: int
  abstract newFile: DiffFile
  abstract nfiles: int
  abstract oldFile: DiffFile
  abstract similarity: int
  abstract status: int

type DiffFile =
  abstract flags: unit -> int
  abstract id: unit -> Oid
  abstract mode: unit -> int
  abstract path: unit -> string
  abstract size: unit -> int

type StatusEntry =
  abstract headToIndex: DiffDelta option
  abstract indexToWorkdir: DiffDelta option
  abstract status: int

type StatusStatic =
  abstract byIndex: StatusList -> int -> StatusEntry

type CheckoutStatic =
  abstract tree: Repository -> Tree -> Promise<unit>

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
  abstract Revparse: RevparseStatic
  abstract StatusOptions: StatusOptionsStatic
  abstract StatusList: StatusListStatic
  abstract Status: StatusStatic
  abstract AnnotatedCommit: AnnotatedCommitStatic
  abstract Tree: TreeStatic
  abstract Checkout: CheckoutStatic

[<Import("*", "nodegit")>]
let nodegit: IExports = Exceptions.jsNative
