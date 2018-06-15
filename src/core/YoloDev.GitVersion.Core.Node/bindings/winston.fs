module rec Bindings.Winston

open Fable.Core
open Fable.Core.JsInterop

[<AbstractClass>]
type Transport private () = class end

type TransportsStatic =
  // Ugly hack, forces "null" -> "undefined"
  [<Emit("new $0.Console($1 || void 0)")>]
  abstract Console: ?opts: LoggerFactoryArgs -> Transport // JsConstructor<unit, Transport>

[<AbstractClass>]
type Format private () = class end

type FormatStatic =
  abstract combine: [<ParamList>] formats: Format list -> Format
  abstract timestamp: unit -> Format
  abstract printf: (ILogMessage -> string) -> Format

[<Pojo>]
type ILogMessage =
  abstract level: string with get, set
  abstract message: string with get, set

type Logger =
  abstract log: ILogMessage -> unit
  abstract clear: unit -> Logger
  abstract add: Transport -> Logger
  abstract remove: Transport -> Logger
  abstract configure: LoggerFactoryArgs -> unit

[<Pojo>]
type LoggerFactoryArgs =
  abstract level: string with get, set
  abstract levels: obj with get, set
  abstract format: Format with get, set
  abstract transports: Transport array with get, set
  abstract exitOnError: bool with get, set
  abstract silent: bool with get, set

type IExports =
  abstract createLogger: LoggerFactoryArgs -> Logger
  abstract format: FormatStatic
  abstract transports: TransportsStatic

[<Import("*", "winston")>]
let winston: IExports = Exceptions.jsNative
