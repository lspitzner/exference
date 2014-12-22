module Language.Haskell.Exference.FunctionDecl
  ( HsFunctionDecl
  , RatedHsFunctionDecl
  , declToBinding
  )
where



import Language.Haskell.Exference.FunctionBinding
import Language.Haskell.Exference.ConstrainedType
import Language.Haskell.Exference.Type



type HsFunctionDecl = (String, HsConstrainedType)
type RatedHsFunctionDecl = (String, Float, HsConstrainedType)
                            -- name, rating, type

declToBinding :: RatedHsFunctionDecl -> FunctionBinding
declToBinding (a,r,HsConstrainedType constrs b) =
  (result, a, r, constrs, params)
 where
  (result, params) = f b
  f :: HsType -> (HsType, [HsType])
  f (TypeArrow t1 t2) = let (c,d) = f t2 in (c, t1:d)
  f t = (t, [])
