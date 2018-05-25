[<AutoOpen>]
module YoloDev.GitVersion.Core.EnvHelpers

open YoloDev.GitVersion.Core.Abstractions
open YoloDev.GitVersion

[<RequireQualifiedAccess>]
module Env =

  let getVar name = IO <| fun sys cont ->
    IO.fork (sys.GetEnv name) sys cont