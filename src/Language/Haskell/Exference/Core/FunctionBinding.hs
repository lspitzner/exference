module Language.Haskell.Exference.Core.FunctionBinding
  ( FunctionBinding
  , DeconstructorBinding
  , EnvDictionary
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.Expression



type FunctionBinding = (HsType, QNameId, Float, [HsConstraint], [HsType])
                      -- input-type, name, rating, contraints, result-types

type DeconstructorBinding = (HsType, [(QNameId, [HsType])], Bool)
                      -- input-type, (name, result-types)s, is-recursive

type EnvDictionary = ( [FunctionBinding]
                     , [DeconstructorBinding]
                     , StaticClassEnv
                     )
