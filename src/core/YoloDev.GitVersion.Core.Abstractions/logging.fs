[<AutoOpen>]
module YoloDev.GitVersion.Core.Logging

open YoloDev.GitVersion.Core.Abstractions
open YoloDev.GitVersion

[<Struct; NoEquality; NoComparison>]
type Logger = 
  #if !NODE
  private 
  #endif
    Logger of IO<ILogger>

[<Struct; NoEquality; NoComparison>]
type LogMessageFactory = 
  #if !NODE
  private 
  #endif
    LogMessageFactory of ILogMessageFactory

[<Struct; NoEquality; NoComparison>]
type LogMessage = 
  #if !NODE
  private 
  #endif
    LogMessage of ILogMessage

[<RequireQualifiedAccess>]
module Logger =

  let private create' name = IO <| fun sys cont ->
    IO.fork (sys.GetLogger name) sys cont
  
  let private wrap (f: LogMessageFactory -> LogMessage) =
    fun (factory: ILogMessageFactory) ->
      let (LogMessage msg) = f (LogMessageFactory factory)
      msg

  let create name =
    create' name
    |> IO.cache
    |> Logger
  
  let verbose (Logger l) f =
    l |> IO.bind (fun logger ->
      logger.LogVerbose (wrap f))
  
  let debug (Logger l) f =
    l |> IO.bind (fun logger ->
      logger.LogDebug (wrap f))
  
  let info (Logger l) f =
    l |> IO.bind (fun logger ->
      logger.LogInfo (wrap f))
  
  let warn (Logger l) f =
    l |> IO.bind (fun logger ->
      logger.LogWarn (wrap f))
  
  let error (Logger l) f =
    l |> IO.bind (fun logger ->
      logger.LogError (wrap f))
  
  let fatal (Logger l) f =
    l |> IO.bind (fun logger ->
      logger.LogFatal (wrap f))

let eventX message = fun (LogMessageFactory factory) -> 
  factory.Event message |> LogMessage

let setField name v = fun (LogMessage msg) ->
  msg.SetField name v |> LogMessage

let addExn (e: #exn) = fun (LogMessage msg) ->
  msg.AddExn e |> LogMessage
