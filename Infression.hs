-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
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
import Control.Monad ( guard, mzero )

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
findExpressions rawCType funcs staticContext =
  let (HsConstrainedType cs t) = ctConstantifyVars rawCType
      r = findExpression' 0 $ Q.singleton 100000.0 $ State
        [((0, t), [])]
        []
        (map splitFunctionType funcs)
        (mkDynContext staticContext cs)
        (ExpHole 0)
        1
        (largestId t)
        0.0
        Nothing
        ""
  in [(e, InfressionStats steps compl) | (steps, compl, e) <- r]

ctConstantifyVars :: HsConstrainedType -> HsConstrainedType
ctConstantifyVars (HsConstrainedType a b) =
  HsConstrainedType
    (map (\(Constraint c d) -> Constraint c $ map tConstantifyVars d) a)
    (tConstantifyVars b)

tConstantifyVars :: HsType -> HsType
tConstantifyVars (TypeVar i)        = TypeCons $ "INF" ++ showVar i
tConstantifyVars c@(TypeCons _)     = c
tConstantifyVars (TypeArrow t1 t2)  = TypeArrow
                                       (tConstantifyVars t1)
                                       (tConstantifyVars t2)
tConstantifyVars (TypeApp t1 t2)    = TypeApp
                                       (tConstantifyVars t1)
                                       (tConstantifyVars t2)
tConstantifyVars f@(TypeForall _ _) = f

findExpression' :: Int -> RatedStates -> [(Int,Float,Expression)]
findExpression' n states =
  if Q.null states
    then []
    else
      let ((_,s), restStates) = Q.deleteFindMax states
      in if null (goals s)
        then if null (constraintGoals s)
          then -- trace (showStateDevelopment s) $
            (n, depth s, expression s) : findExpression' (n+1) restStates
          else findExpression' (n+1) restStates
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
  if depth s > 12.0
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
        (constraintGoals s)
        (functions s)
        (context s)
        (fillExprHole var (ExpLambda v1 (ExpHole v2)) $ expression s)
        newNext
        (maxTVarId s)
        (depth s + 0.5)
        (Just s)
        "function goal transform"
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
            ("inserting given value " ++ show provId ++ "::" ++ show provT)
        byFunction = do
          (funcId, funcR, funcParams, funcConstrs) <- functions s
          let incF = incVarIds (+(1+maxTVarId s))
          byGenericUnify
            (ExpLit funcId)
            (incF funcR)
            (map (constraintMapTypes incF) funcConstrs)
            (map incF funcParams)
            3.0
            ("applying function " ++ show funcId)
        byGenericUnify :: Expression
                       -> HsType
                       -> [Constraint]
                       -> [HsType]
                       -> Float
                       -> String
                       -> [State]
        byGenericUnify coreExp provided provConstrs
                       dependencies depthMod reasonPart = do
          substs <- maybeToList $ unify goalType provided
          let contxt = context s
              constrs1 = map (constraintApplySubsts substs)
                       $ constraintGoals s
              constrs2 = map (constraintApplySubsts substs)
                       $ provConstrs
          case isPossible contxt (constrs1++constrs2) of
            Nothing -> []
            Just newConstraints ->
              let
                substsTxt   = show substs ++ " unifies " ++ show goalType ++ " and " ++ show provided
                provableTxt = "constraints (" ++ show (constrs1++constrs2) ++ ") are provable"
                vBase = nextVarId s
                paramN = length dependencies
                expr = case paramN of
                  0 -> coreExp
                  n -> foldl ExpApply coreExp (map ExpHole [vBase..vBase+n-1])
                newGoals = map (,map (map (varPBindingApplySubsts substs)) binds)
                         $ zip [vBase..]
                               (map (applySubsts substs) dependencies)
              in return $ State
                (newGoals ++ map (goalApplySubst substs) gr)
                newConstraints
                (functions s)
                (context s)
                (fillExprHole var expr $ expression s)
                (vBase + paramN)
                (max (maxTVarId s) (largestSubstsId substs))
                (depth s + depthMod)
                (Just s)
                (reasonPart ++ ", because " ++ substsTxt ++ " and because " ++ provableTxt)

splitFunctionType :: (a, HsConstrainedType)
                  -> (a, HsType, [HsType], [Constraint])
splitFunctionType (a,HsConstrainedType constrs b) =
  let (c,d) = f b in (a,c,d,constrs)
  where
    f :: HsType -> (HsType, [HsType])
    f (TypeArrow t1 t2) = let (c',d') = f t2 in (c', t1:d')
    f t = (t, [])
