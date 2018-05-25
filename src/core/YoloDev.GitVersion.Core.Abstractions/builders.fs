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
  
  let zero = unit ()

  let map f ma = IO <| fun sys cont ->
    fork ma sys (Result.map f >> cont)
  
  let bind f ma = IO <| fun sys cont ->
    let cont' ra = 
      match ra with
      | Ok a -> fork (f a) sys cont
      | Error e -> cont (Error e)

    fork ma sys cont'
  
  let tryFinally (cleanup: unit -> unit) ma = IO <| fun sys cont ->
    let cont' ra =
      cleanup ()
      cont ra
    
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
      f ra
      cont ra
    
    fork ma sys cont'
  
  let tap f = tapResult (function | Error _ -> () | Ok v -> f v)
  
  let combine ma mb =
    bind (fun _ -> mb) ma
  
  [<GeneralizableValue>]
  let toResult ma = IO <| fun sys cont ->
    fork ma sys (Ok >> cont)

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
    cont (Ok ())

  let singleton x = IOSeq <| fun _ next cont ->
    next x (Result.map ignore >> cont)
  
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
                                              | Ok false -> cont (Ok ())
                                              | Error e  -> cont (Error e))
      let cont'' = function | Ok () -> cont' (Ok true) | Error e -> cont' (Error e)
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
      | Error e -> cont (Error e)
      | Ok ()   ->
        if f ()
        then forkSeq ma sys next cont'
        else cont (Ok ())
    
    cont' (Ok ())
  
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
  
  let fold f s ma = IO <| fun sys cont ->
    let mutable state = s
    let cnext a cont' =
      state <- f state a
      cont' (Ok true)
    
    let cont' =
      function
      | Ok ()   -> cont (Ok state)
      | Error e -> cont (Error e)
    
    forkSeq ma sys cnext cont'
  
  let tryFinally (cleanup: unit -> unit) ma = IOSeq <| fun sys next cont ->
    let cont' ra =
      cleanup ()
      cont ra
    
    forkSeq ma sys next cont'
  
  let using f (d: #System.IDisposable) = tryFinally (fun () -> d.Dispose ()) (f d)
  
  let ofIO ma = IOSeq <| fun sys next cont ->
    IO.fork ma sys <|
      function
      | Ok x    -> next x (Result.map ignore >> cont)
      | Error e -> cont (Error e)
  
  let doFirst io seq = IOSeq <| fun sys next cont ->
    IO.fork io sys <|
      function
      | Ok ()   -> forkSeq seq sys next cont
      | Error e -> cont (Error e)

  let ofSeq (s: #(_ seq)) = delay <| fun () ->
    let g = s.GetEnumerator ()
    
    g |> using (fun g -> IOSeq <| fun _ next cont ->
      let err e = cont (Error e)
      let cont () = cont (Ok ())
      
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
        itm <- Some v
        cont (Ok false))
      (function | Ok () -> cont (Ok itm) | Error e -> cont (Error e))
    


[<RequireQualifiedAccess>]
module Builders =

  type IOBuilder () =
    member inline __.Delay f = IO.delay f
    member inline __.Zero () = IO.zero
    member inline __.Return x = IO.unit x
    member inline __.ReturnFrom (ma: #IO<_>) = ma
    member inline __.Bind (ma, f) = IO.bind f ma
    member inline __.For (s, f) = Seq.fold (fun state t -> IO.combine state (IO.delay (fun () -> f t))) IO.zero s
    member inline __.Combine (ma, mb) = IO.combine ma mb
    member inline __.TryFinally (ma, f) = IO.tryFinally f ma
  
  type IOSeqBuilder () =
    member inline __.Delay f = IOSeq.delay f
    member inline __.Zero () = IOSeq.empty
    member inline __.Yield x = IOSeq.singleton x
    member inline __.YieldFrom (ma: #IOSeq<_>) = ma
    member inline __.TryFinally (ma, f) = IOSeq.tryFinally f ma
    member inline __.Bind (ma, f) = IOSeq.bindIO f ma
    member inline __.While (f, ma) = IOSeq.doWhile f ma

let io = Builders.IOBuilder ()
let ioSeq = Builders.IOSeqBuilder ()