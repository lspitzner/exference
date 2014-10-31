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
isPossible :: QueryClassEnv -> [HsConstraint] -> Maybe [HsConstraint]
isPossible _qClassEnv [] = Just []
isPossible qClassEnv (c:constraints) =
  if constraintContainsVariables c
    then (c:) <$> rest
    else do
      cs' <- possibleFromGiven <|> possibleFromInstance
      cs  <- rest
      return $ cs' ++ cs
  where
    rest = isPossible qClassEnv constraints
    (HsConstraint cclass cparams) = c
    possibleFromGiven :: Maybe [HsConstraint]
    possibleFromGiven =
      if S.member c $ qClassEnv_inflatedConstraints qClassEnv
        then Just []
        else Nothing
    possibleFromInstance :: Maybe [HsConstraint]
    possibleFromInstance = msum $ f <$> is
      where
        is = fromMaybe [] $ M.lookup (tclass_name cclass)
                                     ( sClassEnv_instances
                                     $ qClassEnv_env qClassEnv )
        f (HsInstance instConstrs _iclass instParams) = do
          let tempTuplePs     = foldl TypeApp (TypeCons "NTUPLE") cparams
          let tempTupleInstPs = foldl TypeApp (TypeCons "NTUPLE") instParams
          substs <- unifyRight tempTuplePs tempTupleInstPs
          isPossible
            qClassEnv
            (map (constraintApplySubsts substs) instConstrs)
