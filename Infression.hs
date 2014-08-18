-- {-# LANGUAGE TemplateHaskell #-}
module Infression
  ( findExpressions
  , findOneExpression
  , InfressionStats (..)
  )
where



import Type
import Expression
import Unify
import TypeClasses
import ConstrainedType
import ConstraintSolver
import InfressionState
import InfressionStats

import qualified Data.PQueue.Prio.Max as Q
import qualified Data.Map as M
import qualified Data.Set as S

import Control.DeepSeq

import Data.Maybe ( maybeToList, listToMaybe )
import Control.Arrow ( first, second, (***) )
import Control.Monad ( guard )

-- import Data.DeriveTH
import Debug.Hood.Observe
import Debug.Trace



type RatedStates = Q.MaxPQueue Float State

findOneExpression :: HsConstrainedType
                  -> [(String, HsConstrainedType)]
                  -> StaticContext
                  -> Maybe (Expression, InfressionStats)
findOneExpression t avail cont = listToMaybe $ findExpressions t avail cont

findExpressions :: HsConstrainedType
                -> [(String, HsConstrainedType)]
                -> StaticContext
                -> [(Expression, InfressionStats)]
findExpressions (HsConstrainedType cs t) funcs staticContext =
  let r = findExpression' 0 $ Q.singleton 100000.0 $ State
        [((0, t), [])]
        (map splitFunctionType funcs)
        (mkDynContext staticContext cs)
        (ExpHole 0)
        1
        (largestId t)
        0.0
        Nothing
  in [(e, InfressionStats steps compl) | (steps, compl, e) <- r]

findExpression' :: Int -> RatedStates -> [(Int,Float,Expression)]
findExpression' n states =
  if Q.null states
    then []
    else
      let ((_,s), restStates) = Q.deleteFindMax states
      in if null (goals s)
        then -- trace (showStateDevelopment s) $
          (n, depth s, expression s) : findExpression' (n+1) restStates
        else
          let resultStates = stateStep s
          in findExpression' (n+1)
               $ foldr
                   (uncurry Q.insert)
                   restStates
                   [ (r, newS)
                   | newS <- resultStates
                   , let r = rateState newS]

rateState :: State -> Float
rateState s = 0.0 - fromIntegral (length $ goals s) - depth s

stateStep :: State -> [State]
stateStep s = -- traceShow s $
  if depth s > 50.0
    then []
    else case goalType of
      (TypeArrow t1 t2) -> arrowStep t1 t2
      _                 -> normalStep
  where
    (((var, goalType), binds):gr) = goals s
    arrowStep t1 t2 = 
      let v1 = nextVarId s
          v2 = v1+1
          newNext = v2+1
      in (:[]) $ State
        (newGoal (v2, t2) [(v1, t1)] binds:gr)
        (functions s)
        (context s)
        (fillExprHole var (ExpLambda v1 (ExpHole v2)) $ expression s)
        newNext
        (maxTVarId s)
        (depth s + 0.5)
        (Just s)
    normalStep = byProvided ++ byFunction
      where
        byProvided = do
          (provId, provT, provPs) <- concat binds
          byGenericUnify
            (ExpVar provId)
            provT
            (S.toList $ dynContext_constraints $ context s)
            provPs
            0.1
        byFunction = do
          (funcId, funcR, funcParams, funcConstrs) <- functions s
          let incF = incVarIds (+(1+maxTVarId s))
          byGenericUnify
            (ExpLit funcId)
            (incF funcR)
            (map (constraintMapTypes incF) funcConstrs)
            (map incF funcParams)
            3.0
        byGenericUnify :: Expression
                       -> HsType
                       -> [Constraint]
                       -> [HsType]
                       -> Float
                       -> [State]
        byGenericUnify coreExp provided provConstrs dependencies depthMod = do
          substs <- maybeToList $ unify goalType provided
          let contxt = context s
              constrs = map (constraintApplySubsts substs)
                      $ S.toList
                      $ dynContext_constraints
                      $ context s
          guard $ isProvable contxt constrs                     
          let
            vBase = nextVarId s
            paramN = length dependencies
            expr = case paramN of
              0 -> coreExp
              n -> foldl ExpApply coreExp (map ExpHole [vBase..vBase+n-1])
            newGoals = zip 
                         (zip [vBase..]
                              (map (applySubsts substs) dependencies))
                         ( repeat
                         $ map (map (varPBindingApplySubsts substs))
                         $ binds)
            newConstraints = map (constraintApplySubsts substs) provConstrs
            newContext = mkDynContext
              (dynContext_context $ context s)
              (   newConstraints
               ++ (S.toList $ dynContext_constraints $ context s)
              )
          return $ State
            (newGoals ++ map (goalApplySubst substs) gr)
            (functions s)
            newContext
            (fillExprHole var expr $ expression s)
            (vBase + paramN)
            (max (maxTVarId s) (largestSubstsId substs))
            (depth s + depthMod)
            (Just s)

splitFunctionType :: (a, HsConstrainedType)
                  -> (a, HsType, [HsType], [Constraint])
splitFunctionType (a,HsConstrainedType constrs b) =
  let (c,d) = f b in (a,c,d,constrs)
  where
    f :: HsType -> (HsType, [HsType])
    f (TypeArrow t1 t2) = let (c',d') = f t2 in (c', t1:d')
    f t = (t, [])
