module CSharp.Analysis where

import CSharp.AbstractSyntax
import CSharp.Algebra


analysisAlgebra :: CSharpAlgebra Bool () () ()
analysisAlgebra = undefinedAlgebra { clas = \_ _ -> True }

undefinedAlgebra :: CSharpAlgebra a b c d
undefinedAlgebra = CSharpAlgebra
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
