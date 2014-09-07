module Language.Haskell.Exference.FunctionBinding
  ( FunctionBinding
  )
where



import Language.Haskell.Exference.Type
import Language.Haskell.Exference.ConstrainedType
import Language.Haskell.Exference.TypeClasses



type FunctionBinding = (String, Float, HsConstrainedType)
