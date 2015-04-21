module Language.Haskell.Exference.FunctionBinding
  ( FunctionBinding
  , DeconstructorBinding
  , EnvDictionary
  )
where



import Language.Haskell.Exference.Types
import Language.Haskell.Exference.Expression



type FunctionBinding = (HsType, QualifiedName, Float, [HsConstraint], [HsType])
                      -- input-type, name, rating, contraints, result-types

type DeconstructorBinding = (HsType, [(QualifiedName, [HsType])], Bool)
                      -- input-type, (name, result-types)s, is-recursive

type EnvDictionary = ( [FunctionBinding]
                     , [DeconstructorBinding]
                     , StaticClassEnv
                     )
