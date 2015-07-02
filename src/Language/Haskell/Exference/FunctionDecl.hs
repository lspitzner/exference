{-# LANGUAGE PatternGuards #-}

module Language.Haskell.Exference.FunctionDecl
  ( HsFunctionDecl
  , RatedHsFunctionDecl
  , declToBinding
  )
where



import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Expression



type HsFunctionDecl = (QNameId, HsType)
type RatedHsFunctionDecl = (QNameId, Float, HsType)
                            -- name, rating, type

declToBinding :: RatedHsFunctionDecl -> FunctionBinding
declToBinding (a,r,t) =
  (result, a, r, constrs, params)
 where
  (result, params, _, constrs) = splitArrowResultParams t
