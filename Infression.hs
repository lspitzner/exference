-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
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
import Control.Applicative ( (<$>), (<*>) )

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
findExpression' n states
  | Q.null states || n > 50000 = []
  | ((_,s), restStates) <- Q.deleteFindMax states =
    if
      | not $ null (goals s) ->
          findExpression' (n+1) $ foldr
                                       (uncurry Q.insert)
                                       restStates
                                       [ (r, newS)
                                       | newS <- stateStep s
                                       , let r = rateState newS]
      | not $ null (constraintGoals s) ->
          findExpression' (n+1) restStates
      | otherwise -> -- trace (showStateDevelopment s) $
          (n, depth s, expression s) : findExpression' (n+1) restStates

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
        (constraintGoals s)
        (functions s)
        (context s)
        (fillExprHole var (ExpLambda v1 (ExpHole v2)) $ expression s)
        newNext
        (maxTVarId s)
        (depth s + 0.8)
        (Just s)
        "function goal transform"
    normalStep = byProvided ++ byFunctionSimple ++ byMatching
      where
        byProvided = do
          (provId, provT, provPs) <- concat binds
          byGenericUnify
            (ExpVar provId)
            provT
            (S.toList $ dynContext_constraints $ context s)
            provPs
            0.2
            ("inserting given value " ++ show provId ++ "::" ++ show provT)
        byFunctionSimple = do
          SimpleBinding funcId funcR funcParams funcConstrs <- functions s
          let incF = incVarIds (+(1+maxTVarId s))
          byGenericUnify
            (ExpLit funcId)
            (incF funcR)
            (map (constraintMapTypes incF) funcConstrs)
            (map incF funcParams)
            5.0
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
                (maxTVarId s `max` largestSubstsId substs
                             `max` maximum (-1:map largestId dependencies)) -- todo re-structure
                (depth s + depthMod)
                (Just s)
                (reasonPart ++ ", because " ++ substsTxt ++ " and because " ++ provableTxt)
        byMatching = do
          MatchBinding matchId matchRs matchParam <- functions s
          let incF = incVarIds (+(1+maxTVarId s))
              resultTypes = map incF matchRs
              inputType = incF matchParam
          (i,substs) <- take 1 [(i,substs) | (i, Just substs) <- zip [0..] (map (unify goalType) resultTypes)]
          let
            reasonPart = "pattern matching on " ++ matchId
            substsTxt   = show substs ++ " unifies " ++ show goalType ++ " and " ++ show (resultTypes !! i)
            vDep = nextVarId s
            vBase = vDep + 1
            vEnd = vBase + length resultTypes
            expr = ExpLetMatch matchId ([vBase..vEnd-1]) (ExpHole vDep) (ExpVar $ vBase+i)
            newBinds = [(vid, bType, []) | (vid, bType) <- zip [vBase..] resultTypes]
            newGoal :: TGoal
            newGoal = ( (vDep,applySubsts substs inputType)
                      , map (map (varPBindingApplySubsts substs)) $ newBinds:binds
                      )
          return $ State
            (newGoal : map (goalApplySubst substs) gr)
            (map (constraintApplySubsts substs) $ constraintGoals s)
            (functions s)
            (context s)
            (fillExprHole var expr $ expression s)
            vEnd
            (maxTVarId s `max` largestSubstsId substs
                         `max` largestId inputType
                         `max` maximum (-1:map largestId resultTypes))
            (depth s + 0.3)
            (Just s)
            (reasonPart ++ ", because " ++ substsTxt)

splitFunctionType :: (String, HsConstrainedType)
                  -> FuncBinding
                  --  name resultType paramTypes constraints
splitFunctionType (a,HsConstrainedType constrs b) =
  case f b of
    (Left  t,  ps) -> SimpleBinding a t ps constrs
    (Right ts, [p]) -> if null constrs then MatchBinding a ts p
                                       else undefined
    _ -> undefined
  where
    f :: HsType -> (Either HsType [HsType], [HsType])
    f (TypeArrow t1 t2) = let (c',d') = f t2 in (c', t1:d')
    f t  = case g t of
      Nothing -> (Left t, [])
      Just ts -> (Right ts, [])
    g :: HsType -> Maybe [HsType]
    g (TypeCons "INFPATTERN") = Just []
    g (TypeApp t1 t2)         = (++[t2]) <$> g t1
    g _                       = Nothing
