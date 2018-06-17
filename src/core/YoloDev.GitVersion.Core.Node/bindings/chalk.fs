module rec Bindings.Chalk

open Fable.Core
open Fable.Core.JsInterop

type StyleList =
  // Modifiers:
  abstract reset: Style
  abstract bold: Style
  abstract dim: Style
  abstract italic: Style // Not widely supported
  abstract underline: Style
  abstract inverse: Style
  abstract hidden: Style
  abstract strikethrough: Style // Not widely supported
  abstract visible: Style // Text is emitted only if enabled

  // Colors
  abstract black: Style
  abstract red: Style
  abstract green: Style
  abstract yellow: Style
  abstract blue: Style // On Windows the bright version is used since normal blue is illegible
  abstract magenta: Style
  abstract cyan: Style
  abstract white: Style
  abstract gray: Style // "bright black"
  abstract redBright: Style
  abstract greenBright: Style
  abstract yellowBright: Style
  abstract blueBright: Style
  abstract magentaBright: Style
  abstract cyanBright: Style
  abstract whiteBright: Style

  // Background colors
  abstract bgBlack: Style
  abstract bgRed: Style
  abstract bgGreen: Style
  abstract bgYellow: Style
  abstract bgBlue: Style
  abstract bgMagenta: Style
  abstract bgCyan: Style
  abstract bgWhite: Style
  abstract bgBlackBright: Style
  abstract bgRedBright: Style
  abstract bgGreenBright: Style
  abstract bgYellowBright: Style
  abstract bgBlueBright: Style
  abstract bgMagentaBright: Style
  abstract bgCyanBright: Style
  abstract bgWhiteBright: Style

type Style =
  inherit StyleList
  
  [<Emit("$0($1)")>]
  abstract color: string -> string

[<Import("*", "chalk")>]
let chalk: StyleList = Exceptions.jsNative
