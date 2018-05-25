module internal YoloDev.GitVersion.Core.Operators

open YoloDev.GitVersion

module IO =
  let inline (>>=) ma f = IO.bind f ma
  let inline (>=>) f g = f >> IO.bind g
  let inline (>->) f g = f >> IO.map g
  let inline (<.>) ma f = IO.map f ma