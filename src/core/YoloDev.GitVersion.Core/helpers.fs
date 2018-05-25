[<AutoOpen>]
module internal YoloDev.GitVersion.Core.Helpers

open YoloDev.GitVersion
open System.Text.RegularExpressions

let (|Integer|_|) s =
  match global.System.Int32.TryParse s with
  | true, n -> Some n
  | _       -> None

[<RequireQualifiedAccess>]
module Int =

  let tryParse s =
    match System.Int32.TryParse s with
    | true, n -> Some n
    | _       -> None

[<RequireQualifiedAccess>]
module Tuple2 =

  let inline mapRight f (l, r) = (l, f r)

[<RequireQualifiedAccess>]
module Regex =

  let inline replace pattern (replacement: string) str =
    Regex.Replace (str, pattern, replacement
      #if !NODE
      , RegexOptions.CultureInvariant
      #endif
      )
  
  let inline escape str =
    Regex.Escape str

[<RequireQualifiedAccess>]
module Seq =

  let inline tryMaxAndCount seq =
    Seq.fold (fun (count, max) next ->
      match max with
      | None -> count + 1, Some next
      | Some prev ->
        if next > prev 
        then count + 1, Some next
        else count + 1, Some prev) (0, None) seq
  
  let combineAll (check: 'a -> 'b -> 'c option) (innerSeq: 'a seq) (outerSeq: 'b seq) =
    seq {
      use outerEnum = outerSeq.GetEnumerator ()
      let mutable isDone = false
      let mutable set = Set.ofSeq innerSeq

      while outerEnum.MoveNext () && not isDone do
        let outer = outerEnum.Current
        use innerEnum = (set :> seq<_>).GetEnumerator ()

        while innerEnum.MoveNext () && not isDone do
          let inner = innerEnum.Current
          match check inner outer with
          | None -> ()
          | Some v ->
            yield inner, Some v
            set <- Set.remove inner set
            if Set.isEmpty set
            then isDone <- true
      
      for inner in set do
        yield inner, None
    }
  
  let foldsi (folder: 'state -> int -> 'a -> 'state * bool) (state: 'state) (source: 'a seq) =
    use sourceEnum = source.GetEnumerator ()
    let mutable state = state
    let mutable isDone = false
    let mutable index = 0

    while sourceEnum.MoveNext () && not isDone do
      let i = index
      index <- i + 1

      let nextState, completed = folder state i sourceEnum.Current
      state <- nextState
      isDone <- completed
    
    state

[<RequireQualifiedAccess>]
module Map =

  let keys map =
    Map.toSeq map
    |> Seq.map fst
    |> Set.ofSeq
  
  let values map =
    Map.toSeq map
    |> Seq.map snd
    |> Set.ofSeq

[<RequireQualifiedAccess>]
module IO =
  let rec choose mas =
    match mas with
    | [] -> IO.unit None
    | ma :: rest ->
      IO.bind (function | Some a -> IO.unit (Some a)
                         | None   -> choose rest) ma

[<RequireQualifiedAccess>]
module IOSeq =

  let tryMax seq =
    IOSeq.fold (fun max next ->
      match max with
      | None -> Some next
      | Some prev ->
        if next > prev 
        then Some next
        else Some prev) None seq
  
  let inline count seq =
    IOSeq.fold (fun s _ -> s + 1) 0 seq