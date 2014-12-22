module Language.Haskell.Exference.FunctionBinding
  ( FunctionBinding
  , DeconstructorBinding
  , EnvDictionary
  )
where



import Language.Haskell.Exference.Type
import Language.Haskell.Exference.ConstrainedType
import Language.Haskell.Exference.TypeClasses



type FunctionBinding = (HsType, String, Float, [HsConstraint], [HsType])
                      -- input-type, name, rating, contraints, result-types

type DeconstructorBinding = (HsType, [(String, [HsType])], Bool)
                      -- input-type, (name, result-types)s, is-recursive

type EnvDictionary = ( [FunctionBinding]
                     , [DeconstructorBinding]
                     , StaticClassEnv
                     )
