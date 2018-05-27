[<AutoOpen>]
module YoloDev.GitVersion.Semver

open System.Text.RegularExpressions
open System.Globalization
open System.Collections
open System
open System.Linq
open YoloDev.GitVersion.Core

#if NODE
open Fable.Core
#endif

let logger = Logging.Log.create "YoloDev.GitVersion.Semver"

[<AutoOpen>]
module private Helpers =
  [<RequireQualifiedAccess>]
  module Int =
    let parse (s: string) = 
      try System.Int32.Parse (s, CultureInfo.InvariantCulture)
      with :? FormatException -> failwithf "'%s' was not a number" s
  
  let equalityComparer =
    #if !NODE
    LanguagePrimitives.GenericEqualityComparer
    #else
    { new IEqualityComparer with
      member __.GetHashCode x = Unchecked.hash x
      member __.Equals (x, y) = Object.Equals (x, y) }
    #endif
  
  let comparer =
    #if !NODE
    LanguagePrimitives.GenericComparer
    #else
    { new IComparer with
      member __.Compare (x, y) = Unchecked.compare x y }
    #endif
  
  #if NODE
  [<Emit("$0 || $1")>]
  let __safe (_a: 'a) (_b: 'a) : 'a = jsNative
  #endif

  let safeEq (eq: IEqualityComparer) =
    #if !NODE
    eq
    #else
    __safe eq equalityComparer
    #endif
  
  let safeCmp (eq: IComparer) =
    #if !NODE
    eq
    #else
    __safe eq comparer
    #endif

  [<RequireQualifiedAccess>]
  module String =
    let split (del: char) (s: string) = s.Split del |> List.ofArray

  let (|LT|EQ|GT|) n =
    if n < 0
    then LT
    elif n > 0
    then GT
    else EQ
  
  let numEx =
    Regex (@"^\d+$"
      #if !NODE
      , RegexOptions.CultureInvariant ||| RegexOptions.Compiled
      #endif
      )
  
  let (|Number|String|) s =
    match numEx.Match s with
    | null                 -> String s
    | m when not m.Success -> String s
    | _                    -> Number (Int.parse s)

[<CustomEquality; CustomComparison>]
type Segment =
  | Num of int
  | Str of string

  override s.ToString () =
    match s with
    | Num i -> string i
    | Str s -> s

  member s.GetHashCode (cmp: IEqualityComparer) =
    let cmp = safeEq cmp
    match s with
    | Num n -> cmp.GetHashCode <| (0, n)
    | Str s -> cmp.GetHashCode <| (1, s)

  override s.GetHashCode () =
    s.GetHashCode equalityComparer

  member s1.Equals (o: obj, comparer: IEqualityComparer) =
    let comparer = safeEq comparer
    match o with
    | :? Segment as s2 ->
      match s1, s2 with
      | Num n1, Num n2 -> comparer.Equals (n1, n2)
      | Str s1, Str s2 -> comparer.Equals (s1, s2)
      | _              -> false
    | _ -> false

  override s1.Equals (o: obj) =
    s1.Equals (o, equalityComparer)
  
  member s1.CompareTo (o: obj, comparer: IComparer) =
    let comparer = safeCmp comparer
    match o with
    | :? Segment as s2 ->
      match s1, s2 with
      | Num n1, Num n2 -> comparer.Compare (n1, n2)
      | Str s1, Str s2 -> comparer.Compare (s1, s2)
      | Num _, Str _   -> -1
      | Str _, Num _   ->  1
    | _ -> invalidArg "object" "Object must be of type segment."

  member s1.CompareTo (o: obj) =
    s1.CompareTo (o, comparer)
  
  member s1.CompareTo (s2: Segment) =
    s1.CompareTo (s2, comparer)

#if !NODE
  interface System.IComparable<Segment> with
    member s1.CompareTo s2 = s1.CompareTo s2
  
  interface System.IComparable with
    member s1.CompareTo s2 = s1.CompareTo s2
#endif

  interface IStructuralComparable with
    member s1.CompareTo (s2, comparer) = s1.CompareTo (s2, comparer)

#if !NODE
  interface System.IEquatable<Segment> with
    member s1.Equals s2 = s1.Equals s2
#endif

  interface IStructuralEquatable with
    member s1.Equals (o, comparer) = s1.Equals (o, comparer)
    member s1.GetHashCode comparer = s1.GetHashCode comparer

[<AutoOpen>]
module private SegmentHelpers =
  let compareMeta v1 v2 (comparer: IComparer) =
    let comparer = safeCmp comparer
    let rec cmp l1 l2 =
      match l1, l2 with
      | []  , []   ->  0
      | _::_, []   ->  1
      | []  , _::_ -> -1
      | h1::t1, h2::t2 ->
        match comparer.Compare (h1, h2) with
        | LT -> -1
        | GT ->  1
        | EQ -> cmp t1 t2
    
    cmp v1 v2
  
  let parseMeta (name: string) (meta: string) =
    match meta with
    | "" -> Ok []
    | meta ->
      let parts = String.split '.' meta
      if List.exists String.IsNullOrEmpty parts 
      then Error <| sprintf "%s has empty segment" name
      else
        parts
        |> List.map (function | Number n -> Num n | String s -> Str s)
        |> Ok

[<CustomEquality; CustomComparison>]
[<StructuredFormatDisplay("{VersionString}")>]
type Semver =
  { major: int
    minor: int
    patch: int
    pre: Segment list
    build: Segment list }
  
  member v.VersionString =
    let pre =
      match v.pre with
      | [] -> ""
      | pre ->
        pre
        |> List.map string
        |> String.concat "."
        |> sprintf "-%s"
    
    let build =
      match v.build with
      | [] -> ""
      | build ->
        build
        |> List.map string
        |> String.concat "."
        |> sprintf "+%s"
    
    sprintf "%d.%d.%d%s%s" v.major v.minor v.patch pre build
  
  override v.ToString () = v.VersionString

  member v.GetHashCode (cmp: IEqualityComparer) =
    cmp.GetHashCode <| (v.major, v.minor, v.patch, v.pre, v.build)

  override v.GetHashCode () =
    v.GetHashCode equalityComparer

  member v1.Equals (o: obj, comparer: IEqualityComparer) =
    let comparer = safeEq comparer
    match o with
    | :? Semver as v2 ->
      comparer.Equals (v1.major, v2.major) &&
      comparer.Equals (v1.minor, v2.minor) &&
      comparer.Equals (v1.patch, v2.patch) &&
      comparer.Equals (v1.pre, v2.pre) &&
      comparer.Equals (v1.build, v2.build)
    | _ -> false

  override v1.Equals (o: obj) =
    v1.Equals (o, equalityComparer)
  
  member v1.CompareTo (o: obj, comparer: IComparer) =
    let comparer = safeCmp comparer
    match o with
    | :? Semver as v2 ->
      match comparer.Compare (v1.major, v2.major) with
      | LT -> -1
      | GT ->  1
      | EQ ->
        match comparer.Compare (v1.minor, v2.minor) with
        | LT -> -1
        | GT ->  1
        | EQ ->
          match comparer.Compare (v1.patch, v2.patch) with
          | LT -> -1
          | GT ->  1
          | EQ ->
            match v1.pre, v2.pre with
            | [], _::_ ->  1
            | _::_, [] -> -1
            | _ ->
              match compareMeta v1.pre v2.pre comparer with
              | LT -> -1
              | GT ->  1
              | EQ ->
                match v1.build, v2.build with
                | [], _::_ ->  1
                | _::_, [] -> -1
                | [], []   ->  0
                | _ ->
                  match compareMeta v1.build v2.build comparer with
                  | LT -> -1
                  | GT ->  1
                  | EQ ->  0
    | _ -> invalidArg "object" "Object must be of type semver."
  
  member v1.CompareTo (o: obj) =
    v1.CompareTo (o, comparer)
  
  member v1.CompareTo (v2: Semver) =
    v1.CompareTo (v2, comparer)

#if !NODE
  interface System.IComparable<Semver> with
    member v1.CompareTo v2 = v1.CompareTo v2
  
  interface System.IComparable with
    member v1.CompareTo v2 = v1.CompareTo v2
#endif

  interface IStructuralComparable with
    member v1.CompareTo (v2, comparer) = v1.CompareTo (v2, comparer)

#if !NODE
  interface System.IEquatable<Semver> with
    member v1.Equals v2 = v1.Equals v2
#endif  

  interface IStructuralEquatable with
    member v1.Equals (o, comparer) = v1.Equals (o, comparer)
    member v1.GetHashCode comparer = v1.GetHashCode comparer

[<AutoOpen>]
module Parsing =

  type Groups =
    | Major = 1
    | Minor = 2
    | Patch = 3
    | Pre = 4
    | Build = 5


  let private regexStr =
    @"^(\d+)" +
    @"(?:\.(\d+))" +
    @"(?:\.(\d+))" +
    @"(?:\-([0-9A-Za-z\-\.]+))?" +
    @"(?:\+([0-9A-Za-z\-\.]+))?$"
  
  [<RequireQualifiedAccess>]
  module private Match =
    let group (g: Groups) (m: Match) = m.Groups.[int g].Value
    let groupOrDefault (g: Groups) (defaultVal: string) (m: Match) =
      let group = m.Groups.[int g]
      match group.Success with
      | false -> defaultVal
      | true  -> group.Value
  
  [<RequireQualifiedAccess>]
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Semver =
    let mkParser mkRegex mkResult =
      let regex = Regex (mkRegex regexStr
        #if !NODE
        , RegexOptions.CultureInvariant ||| RegexOptions.Compiled
        #endif
        )
      
      fun version ->
        let parseMatch = regex.Match version
        match parseMatch with
        | null -> Error "Invalid version."
        | m ->
          let major = m |> Match.group Groups.Major |> Int.parse
          let minor = m |> Match.group Groups.Minor |> Int.parse
          let patch = m |> Match.group Groups.Patch |> Int.parse
          let pre = m |> Match.groupOrDefault Groups.Pre "" |> parseMeta "pre"
          let build = m |> Match.groupOrDefault Groups.Build "" |> parseMeta "build"
          match pre, build with
          | Ok pre, Ok build ->
            let semver =
              { major = major
                minor = minor
                patch = patch
                pre = pre
                build = build }
            
            mkResult m semver
          | Error e, _       -> Error e
          | _      , Error e -> Error e

    let attemptParse = mkParser id (fun _ s -> Ok s)

    let tryParse =
      attemptParse 
      >> function | Ok v -> Some v | _ -> None

    let parse = 
      attemptParse
      >> function | Ok v -> v | Error e -> invalidArg "version" e
