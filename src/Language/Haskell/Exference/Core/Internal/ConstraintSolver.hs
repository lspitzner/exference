module Language.Haskell.Exference.Core.Internal.ConstraintSolver
  ( isPossible
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Internal.Unify

import qualified Data.Set as S
import Control.Monad ( mplus, guard, msum )
import Control.Applicative ( (<|>), (<$>), liftA2 )
import Control.Monad ( join )
import Data.Maybe ( fromMaybe )

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IntMap

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
        is = fromMaybe [] $ IntMap.lookup (tclass_name cclass)
                                          ( sClassEnv_instances
                                          $ qClassEnv_env qClassEnv )
        f (HsInstance instConstrs _iclass instParams) = do
          -- behold the undocumented special -14. TODO document, or add
          -- special constructor..
          let tempTuplePs     = foldl TypeApp (TypeConstant (-14)) cparams
          let tempTupleInstPs = foldl TypeApp (TypeConstant (-14)) instParams
          substs <- unifyRight tempTuplePs tempTupleInstPs
          isPossible
            qClassEnv
            (map (constraintApplySubsts substs) instConstrs)
