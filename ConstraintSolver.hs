module ConstraintSolver where



import Type
import Unify
import TypeClasses

import qualified Data.Set as S
import Control.Monad ( mplus, guard )



isProvable :: DynContext -> [Constraint] -> Bool
isProvable _ [] = True
isProvable dcontext (c1:constraints) =
  let
    provableFromContext :: Constraint -> Bool
    provableFromContext c = and
      [ S.member c $ inflateConstraints
                                (dynContext_context dcontext)
                                (dynContext_constraints dcontext)
      , isProvable dcontext constraints
      ]
    provableFromInstance :: Constraint -> Bool
    provableFromInstance (Constraint c ps) = or $ do
      HsInstance instConstrs inst instParams <- context_instances 
                                              $ dynContext_context dcontext
      guard $ inst==c
      let tempTuplePs     = foldl TypeApp (TypeCons "NTUPLE") ps
          tempTupleInstPs = foldl TypeApp (TypeCons "NTUPLE") instParams
      case unifyRight tempTuplePs tempTupleInstPs of -- or other way round?
        Nothing     -> []
        Just substs ->
          return $ isProvable dcontext
                 $ [constraintApplySubsts substs instC | instC <- instConstrs] ++ constraints
  in
    provableFromContext c1 || provableFromInstance c1
