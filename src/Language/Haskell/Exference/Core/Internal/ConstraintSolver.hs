{-# LANGUAGE MonadComprehensions #-}

module Language.Haskell.Exference.Core.Internal.ConstraintSolver
  ( isPossible
  , filterUnresolved
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Internal.Unify

import qualified Data.Set as S
import Control.Monad ( mplus, guard )
import Control.Applicative ( (<|>), (<$>), liftA2 )
import Data.Foldable ( asum )
import Control.Monad ( join )
import Data.Maybe ( fromMaybe )

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IntMap

import Debug.Trace



-- returns Nothing if one of the constraints is refutable
-- otherwise Just a list of open constraints (that could neither be
-- solved nor refuted).
isPossible :: QueryClassEnv -> [HsConstraint] -> Maybe [HsConstraint]
isPossible = checkPossibleGeneric (Just . pure) (const Nothing)

-- returns Nothing if any of the constraints contains variables
-- (as variables cannot be proven).
-- Otherwise returns the list of constraints that cannot be proven,
-- and removing all those that can be proven.
filterUnresolved :: QueryClassEnv -> [HsConstraint] -> Maybe [HsConstraint]
filterUnresolved = checkPossibleGeneric (const Nothing) (Just . pure)

checkPossibleGeneric :: (HsConstraint -> Maybe [HsConstraint])
                     -> (HsConstraint -> Maybe [HsConstraint])
                     -> QueryClassEnv
                     -> [HsConstraint]
                     -> Maybe [HsConstraint]
checkPossibleGeneric containsVarsResult otherwiseResult qClassEnv = fmap concat . traverse g where
  g c = if constraintContainsVariables c
    then containsVarsResult c
    else possibleFromGiven c <|> possibleFromInstance c <|> otherwiseResult c
  possibleFromGiven :: HsConstraint -> Maybe [HsConstraint]
  possibleFromGiven c = [ [] | S.member c $ qClassEnv_inflatedConstraints qClassEnv ]
  possibleFromInstance :: HsConstraint -> Maybe [HsConstraint]
  possibleFromInstance c = asum $ f <$> is where
    (HsConstraint cclass cparams) = c
    is = IntMap.findWithDefault []
                                (tclass_name cclass)
                                ( sClassEnv_instances
                                  $ qClassEnv_env qClassEnv )
    f (HsInstance instConstrs _iclass instParams) = do
      substs <- unifyRightEqs $ zipWith TypeEq cparams instParams
      checkPossibleGeneric containsVarsResult otherwiseResult
        qClassEnv
        (map (snd . constraintApplySubsts substs) instConstrs)
