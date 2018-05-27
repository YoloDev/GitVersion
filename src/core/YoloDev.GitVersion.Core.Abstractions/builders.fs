[<AutoOpen>]
module YoloDev.GitVersion.SystemBuilders

open YoloDev.GitVersion.Core.Abstractions

[<AutoOpen>]
module internal Helpers =

  let once name f =
    let mutable ran = false
    fun () ->
      if ran
      then failwithf "Function %s can only be called once" name
      else f ()
  
  [<RequireQualifiedAccess>]
  module Result =
    let attempt f v =
      try Ok (f v)
      with e -> Error e

    let mapAttempt f v =
      try
        Result.map f v
      with e -> Error e

[<RequireQualifiedAccess>]
module IO =

  let internal fork (g: IO<_>) = g.Fork

  let delay f = IO <| fun sys cont ->
    let maR =
      try Ok (f ())
      with e -> Error e
    match maR with
    | Ok ma   -> fork ma sys cont
    | Error e -> cont (Error e)

  let unit x = IO <| fun _ cont -> 
    cont (Ok x)
  
  let fail e = IO <| fun _ cont ->
    cont (Error e)
  
  let zero = unit ()

  let map f ma = IO <| fun sys cont ->
    fork ma sys (Result.mapAttempt f >> cont)
  
  let bind f ma = IO <| fun sys cont ->
    let cont' ra = 
      match ra with
      | Ok a -> 
        let mb = Result.attempt f a
        match mb with
        | Ok mb -> fork mb sys cont
        | Error e -> cont (Error e)
      | Error e -> cont (Error e)

    fork ma sys cont'
  
  let tryFinally (cleanup: unit -> unit) ma = IO <| fun sys cont ->
    let cont' ra =
      match Result.attempt cleanup () with
      | Ok ()   -> cont ra
      | Error e -> cont (Error e)
    
    fork ma sys cont'
 
  let catchBind (f: exn -> IO<'a>) ma = IO <| fun sys cont ->
    let cont' ra =
      match ra with
      | Ok a    -> cont (Ok a)
      | Error e ->
        match Result.attempt f e with
        | Ok me   -> fork me sys cont
        | Error e -> cont (Error e)
    
    fork ma sys cont'
  
  let using f (d: #System.IDisposable) = tryFinally (fun () -> d.Dispose ()) (f d)

  // Note: We're assuming sys will allways be the same for one system
  let cache ma =
    let mutable result = None
    let mutable listeners = None
    IO <| fun sys cont ->
      match result with
      | Some r -> cont r
      | None   ->
        match listeners with
        | Some list ->
          listeners <- Some (cont :: list)
          FakeUnit
        | None      ->
          listeners <- Some [cont]

          fork ma sys <| fun ra ->
            result <- Some ra
            let list = Option.get listeners
            listeners <- None
            List.iter (fun l -> l ra |> ignore) list

            FakeUnit

  let tapResult f ma = IO <| fun sys cont ->
    let cont' ra =
      match Result.attempt f ra with
      | Ok () -> cont ra
      | Error e -> cont (Error e)
    
    fork ma sys cont'
  
  let tap f = tapResult (function | Error _ -> () | Ok v -> f v)
  
  let combine ma mb =
    bind (fun _ -> mb) ma
  
  [<GeneralizableValue>]
  let toResult ma = IO <| fun sys cont ->
    fork ma sys (Ok >> cont)
  
  let foreach f s =
    Seq.fold (fun state t -> combine state (delay (fun () -> f t))) zero s
 
  let ofAsync (a: Async<_>) =
    IO <| fun _ cont ->
      Async.StartWithContinuations (a, 
        Ok >> cont >> ignore, // If OK
        Error >> cont >> ignore, // If errored
        unbox >> Error >> cont >> ignore) // If cancelled (treat as error)
      FakeUnit

// TODO: There are likley A LOT of places where
// errors can fall through here :(
[<RequireQualifiedAccess>]
module IOSeq =

  let internal forkSeq (g: IOSeq<_>) = g.ForkSeq

  let delay f = IOSeq <| fun sys next cont ->
    let maR =
      try Ok (f ())
      with e -> Error e
    match maR with
    | Ok ma   -> forkSeq ma sys next cont
    | Error e -> cont (Error e)

  [<GeneralizableValue>]
  let empty<'a> : IOSeq<'a> = IOSeq <| fun _ _ cont ->
    cont (Ok true)

  let singleton x = IOSeq <| fun _ next cont ->
    next x cont
  
  let map f ma = IOSeq <| fun sys next cont ->
    forkSeq ma sys (f >> next) cont
  
  let bindIO f ma = IOSeq <| fun sys next cont ->
    IO.fork ma sys <|
      function
      | Ok a    -> forkSeq (f a) sys next cont
      | Error e -> cont (Error e)
  
  let mapM f ma = IOSeq <| fun sys next cont ->
    let next' v cont' =
      IO.fork (f v) sys <|
        function
        | Ok v    -> next v cont'
        | Error e -> cont (Error e)
    
    forkSeq ma sys next' cont
  
  let collect f ma = IOSeq <| fun sys next cont ->
    let next' a cont' = 
      let cnext'' b cont'' = next b (function | Ok true  -> cont'' (Ok true)
                                              | Ok false -> cont (Ok false)
                                              | Error e  -> cont (Error e))

      let cont'' = function | Ok getNext -> cont' (Ok getNext) 
                            | Error e -> cont' (Error e)

      forkSeq (f a) sys cnext'' cont''
    
    forkSeq ma sys next' cont
  
  let choose f ma = IOSeq <| fun sys next cont ->
    let cnext' ma cont' =
      match f ma with
      | Some a -> next a cont'
      | None   -> cont' (Ok true)
    
    forkSeq ma sys cnext' cont
  
  let chooseM f ma = IOSeq <| fun sys next cont ->
    let cnext' ma cont' =
      IO.fork (f ma) sys <|
        function
        | Ok (Some a) -> next a cont'
        | Ok (None  ) -> cont' (Ok true)
        | Error e     -> cont' (Error e)
    
    forkSeq ma sys cnext' cont
  
  let filter f ma = IOSeq <| fun sys next cont ->
    let cnext' a cont' =
      if f a
      then next a cont'
      else cont' (Ok true)
    
    forkSeq ma sys cnext' cont
  
  let doWhile f ma = IOSeq <| fun sys next cont ->
    let rec cont' r =
      match r with
      | Error e    -> cont (Error e)
      | Ok false   -> cont (Ok false)
      | Ok true    ->
        if f ()
        then forkSeq ma sys next cont'
        else cont (Ok true)
    
    cont' (Ok true)
  
  let takeWhile f ma = IOSeq <| fun sys next cont ->
    let cnext' a cont' =
      if f a
      then next a cont'
      else cont' (Ok false)
    
    forkSeq ma sys cnext' cont
  
  let takeWhileM f ma = IOSeq <| fun sys next cont ->
    let cnext' a cont' =
      IO.fork (f a) sys <|
        function
        | Ok true  -> next a cont'
        | Ok false -> cont' (Ok false)
        | Error e  -> cont' (Error e)
    
    forkSeq ma sys cnext' cont
  
  let takeUntilIncludingM f ma = IOSeq <| fun sys next cont ->
    let mutable matched = false
    let next' a cont' =
      if matched
      then cont' (Ok false)
      else 
        IO.fork (f a) sys <|
          function
          | Ok false -> next a cont'
          | Ok true  ->
            matched <- true
            next a cont'
          | Error e -> cont' (Error e)
    
    forkSeq ma sys next' cont
  
  let fold f s ma = IO <| fun sys cont ->
    let mutable state = s
    let cnext a cont' =
      state <- f state a
      cont' (Ok true)
    
    let cont' =
      function
      | Ok _    -> cont (Ok state)
      | Error e -> cont (Error e)
    
    forkSeq ma sys cnext cont'
  
  let tryFinally (cleanup: unit -> unit) ma = IOSeq <| fun sys next cont ->
    let cont' ra =
      match Result.attempt cleanup () with
      | Ok ()   -> cont ra
      | Error e -> cont (Error e)
    
    forkSeq ma sys next cont'
  
  let using f (d: #System.IDisposable) = tryFinally (fun () -> d.Dispose ()) (f d)

  
  let doFirst io seq = IOSeq <| fun sys next cont ->
    IO.fork io sys <|
      function
      | Ok ()   -> forkSeq seq sys next cont
      | Error e -> cont (Error e)

  let ofSeq (s: #(_ seq)) = delay <| fun () ->
    let g = s.GetEnumerator ()
    
    g |> using (fun g -> IOSeq <| fun _ next cont ->
      let err e = cont (Error e)
      let cont () = cont (Ok true)
      
      let rec move () =
        if not (g.MoveNext ())
        then cont ()
        else
          next g.Current <|
            function
            | Ok true  -> move ()
            | Ok false -> cont ()
            | Error e  -> err e
      
      move ())
  
  let tryHead (s: #IOSeq<_>) = IO <| fun sys cont ->
    let mutable itm = None
    forkSeq s sys 
      (fun v cont ->
        // TODO: Remove once fixed
        match itm with
        | Some _ -> failwithf "seq:next called more times than expected!"
        | None   -> ()
        itm <- Some v
        cont (Ok false))
      (function | Ok _ -> cont (Ok itm) | Error e -> cont (Error e))
  
  let foreach (f: 'a -> #IOSeq<'b>) (s: #seq<'a>): IOSeq<'b> =
    ofSeq s |> collect f
  
  let catchBind (f: exn -> #IOSeq<'t>) (s: #IOSeq<'t>) = IOSeq <| fun sys next cont ->
    let cont' r =
      match r with
      | Ok wantMore -> cont (Ok wantMore)
      | Error e     ->
        match Result.attempt f e with
        | Ok mb   -> forkSeq mb sys next cont
        | Error e -> cont (Error e)
    
    forkSeq s sys next cont'
  
  let concat (ma: #IOSeq<'t>) (mb: #IOSeq<'t>) = IOSeq <| fun sys next cont ->
    let cont' r =
      match r with
      | Error e  -> cont (Error e)
      | Ok false -> cont (Ok false)
      | Ok true  -> forkSeq mb sys next cont
    
    forkSeq ma sys next cont'


[<RequireQualifiedAccess>]
module Builders =

  type IOBuilder () =
    member inline __.Delay f = IO.delay f
    member inline __.Zero () = IO.zero
    member inline __.Return x = IO.unit x
    member inline __.ReturnFrom (ma: #IO<_>) = ma
    member inline __.Bind (ma, f) = IO.bind f ma
    member inline __.For (s, f) = IO.foreach f s
    member inline __.For (s, f: _ -> #IO<unit>) = IOSeq.mapM f s |> IO.map ignore
    member inline __.Combine (ma, mb) = IO.combine ma mb
    member inline __.TryFinally (ma, f) = IO.tryFinally f ma
    member inline __.TryWith (ma, f) = IO.catchBind f ma
    member inline __.Using (r, f) = IO.using f r
  
  type IOSeqBuilder () =
    member inline __.Delay f = IOSeq.delay f
    member inline __.Zero () = IOSeq.empty
    member inline __.Yield x = IOSeq.singleton x
    member inline __.YieldFrom (ma: #IOSeq<_>) = ma
    member inline __.TryFinally (ma, f) = IOSeq.tryFinally f ma
    member inline __.TryWith (ma, f) = IOSeq.catchBind f ma
    member inline __.Bind (ma, f) = IOSeq.bindIO f ma
    member inline __.While (f, ma) = IOSeq.doWhile f ma
    member inline __.For (s, f) = IOSeq.foreach f s
    member inline __.Combine (ma, mb) = IOSeq.concat ma mb

let io = Builders.IOBuilder ()
let ioSeq = Builders.IOSeqBuilder ()

open YoloDev.GitVersion.Core.Logging

type Logger with
  member l.verboseIO = l.verboseWithBP >> IO.ofAsync
  member l.debugIO = l.debugWithBP >> IO.ofAsync
  member l.infoIO = l.infoWithBP >> IO.ofAsync
  member l.warnIO = l.warnWithBP >> IO.ofAsync
  member l.errorIO = l.errorWithBP >> IO.ofAsync
  member l.fatalIO = l.fatalWithBP >> IO.ofAsync
