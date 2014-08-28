module Language.Haskell.Exference.Internal.ConstraintSolver
  ( isProvable
  , isPossible
  )
where



import Language.Haskell.Exference.Type
import Language.Haskell.Exference.Internal.Unify
import Language.Haskell.Exference.TypeClasses

import qualified Data.Set as S
import Control.Monad ( mplus, guard, msum )
import Control.Applicative ( (<$>), liftA2 )

import Debug.Trace



-- returns Nothing if one of the constraints is refutable
-- otherwise Just a list of open constraints (that could neither be
-- solved nor refuted).
isPossible :: DynContext -> [Constraint] -> Maybe [Constraint]
isPossible _dcontext [] = Just []
isPossible dcontext (c:constraints) =
  liftA2 (++) (possibleFromGiven `mplus` possibleFromInstance) rest
  where
    rest = isPossible dcontext constraints
    (Constraint cname cparams) = c
    possibleFromGiven =
      if constraintContainsVariables c
      then Just [c]
      else
        if S.member c $ inflateConstraints
                                  (dynContext_context dcontext)
                                  (dynContext_constraints dcontext)
        then Just []
        else Nothing
    possibleFromInstance :: Maybe [Constraint]
    possibleFromInstance = msum $ do
      HsInstance instConstrs inst instParams <- context_instances 
                                              $ dynContext_context dcontext
      guard $ inst==cname
      return $ do
        let tempTuplePs     = foldl TypeApp (TypeCons "NTUPLE") cparams
        let tempTupleInstPs = foldl TypeApp (TypeCons "NTUPLE") instParams
        substs <- unifyRight tempTuplePs tempTupleInstPs
        isPossible
          dcontext
          (map (constraintApplySubsts substs) instConstrs)
       







isProvable :: DynContext -> [Constraint] -> Bool
isProvable _ [] = True
isProvable dcontext (c1:constraints) =
  provableFromContext c1 || provableFromInstance c1
  where
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
                 $    [constraintApplySubsts substs instC
                      | instC <- instConstrs
                      ]
                   ++ constraints
