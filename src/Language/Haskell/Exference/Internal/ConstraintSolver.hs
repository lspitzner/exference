module Language.Haskell.Exference.Internal.ConstraintSolver
  ( isPossible
  )
where



import Language.Haskell.Exference.Type
import Language.Haskell.Exference.Internal.Unify
import Language.Haskell.Exference.TypeClasses

import qualified Data.Set as S
import Control.Monad ( mplus, guard, msum )
import Control.Applicative ( (<|>), (<$>), liftA2 )
import Control.Monad ( join )
import Data.Maybe ( fromMaybe )

import qualified Data.Map.Strict as M

import Debug.Trace



-- returns Nothing if one of the constraints is refutable
-- otherwise Just a list of open constraints (that could neither be
-- solved nor refuted).
isPossible :: DynContext -> [Constraint] -> Maybe [Constraint]
isPossible _dcontext [] = Just []
isPossible dcontext (c:constraints) =
  if constraintContainsVariables c
    then (c:) <$> rest
    else do
      cs' <- possibleFromGiven <|> possibleFromInstance
      cs  <- rest
      return $ cs' ++ cs
  where
    rest = isPossible dcontext constraints
    (Constraint cclass cparams) = c
    possibleFromGiven :: Maybe [Constraint]
    possibleFromGiven =
      if S.member c $ dynContext_inflatedConstraints dcontext
        then Just []
        else Nothing
    possibleFromInstance :: Maybe [Constraint]
    possibleFromInstance = msum $ f <$> is
      where
        is = fromMaybe [] $ M.lookup (tclass_name cclass)
                                     ( context_instances
                          $ dynContext_context dcontext )
        f (HsInstance instConstrs _iclass instParams) = do
          let tempTuplePs     = foldl TypeApp (TypeCons "NTUPLE") cparams
          let tempTupleInstPs = foldl TypeApp (TypeCons "NTUPLE") instParams
          substs <- unifyRight tempTuplePs tempTupleInstPs
          isPossible
            dcontext
            (map (constraintApplySubsts substs) instConstrs)
       






{-
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
-}