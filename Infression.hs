-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}

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

import Data.Maybe ( maybeToList, listToMaybe, fromMaybe )
import Control.Arrow ( first, second, (***) )
import Control.Monad ( guard, mzero )
import Control.Applicative ( (<$>), (<*>) )
import Data.List ( partition )

-- import Data.DeriveTH
import Debug.Hood.Observe
import Debug.Trace



-- the heuristic input factor constant thingies:
factorGoalVar, factorGoalCons, factorGoalArrow, factorGoalApp :: Float
factorStepEnvGood, factorStepProvidedGood, factorStepProvidedBad, factorStepEnvBad :: Float

factorGoalVar   = 5.0
factorGoalCons  = 0.55
factorGoalArrow = 5.5
factorGoalApp   = 1.95
factorStepProvidedGood = 0.2
factorStepProvidedBad  = 6.0
factorStepEnvGood = 6.0
factorStepEnvBad  = 16.0

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
        [((0, t), 0)]
        []
        initialScopes
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
  | Q.null states || n > 100000 = []
  | ((_,s), restStates) <- Q.deleteFindMax states =
    let (potentialSolutions, futures) = partition (null.goals) 
                                                  (stateStep s)
        out = [(n, d, e) | solution <- potentialSolutions,
                           null (constraintGoals solution),
                           let d = depth solution,
                           let e = expression solution]
        rest = findExpression' (n+1) $ foldr 
                   (uncurry Q.insert)
                   restStates
                   [ (r, newS) | newS <- futures
                               , let r = rateState newS ]
    in out ++ rest

rateState :: State -> Float
rateState s = 0.0 - rateGoals (goals s) - depth s

rateGoals :: [TGoal] -> Float
rateGoals = sum . map rateGoal
  where
    rateGoal ((_,t),_) = tComplexity t
    -- TODO: actually measure performance with different values,
    --       use derived values instead of (arbitrarily) chosen ones.
    tComplexity (TypeVar _)       = factorGoalVar
    tComplexity (TypeCons _)      = factorGoalCons
    tComplexity (TypeArrow t1 t2) = factorGoalArrow + tComplexity t1 + tComplexity t2
    tComplexity (TypeApp t1 t2)   = factorGoalApp + tComplexity t1 + tComplexity t2
    tComplexity (TypeForall _ t1) = tComplexity t1

stateStep :: State -> [State]
stateStep s = -- traceShow s $
  stateStep2 s

stateStep2 :: State -> [State]
stateStep2 s
  | depth s > 200.0 = []
  | (TypeArrow t1 t2) <- goalType = arrowStep t1 t2
  | otherwise = byProvided ++ byFunctionSimple
  where
    (((var, goalType), scopeId):gr) = goals s
    arrowStep t1 t2 = 
      let v1 = nextVarId s
          v2 = v1+1
          newNext = v2+1
          (newGoal, newScopeId, newScopes) = addNewScopeGoal scopeId (v2, t2)
                                           $ providedScopes s
      in return $ addScopePatternMatch v2 newScopeId [splitBinding (v1,t1)] $ State
        (newGoal:gr)
        (constraintGoals s) 
        newScopes
        (functions s)
        (context s)
        (fillExprHole var (ExpLambda v1 (ExpHole v2)) $ expression s)
        newNext
        (maxTVarId s)
        (depth s + 0.8)
        (Just s)
        "function goal transform"
    byProvided = do
      (provId, provT, provPs) <- scopeGetAllBindings (providedScopes s) scopeId
      byGenericUnify
        (ExpVar provId)
        provT
        (S.toList $ dynContext_constraints $ context s)
        provPs
        factorStepProvidedGood
        factorStepProvidedBad
        ("inserting given value " ++ show provId ++ "::" ++ show provT)
    byFunctionSimple = do
      SimpleBinding funcId funcR funcParams funcConstrs <- functions s
      let incF = incVarIds (+(1+maxTVarId s))
      byGenericUnify
        (ExpLit funcId)
        (incF funcR)
        (map (constraintMapTypes incF) funcConstrs)
        (map incF funcParams)
        factorStepEnvGood
        factorStepEnvBad
        ("applying function " ++ show funcId)
    byGenericUnify :: Expression
                   -> HsType
                   -> [Constraint]
                   -> [HsType]
                   -> Float
                   -> Float
                   -> String
                   -> [State]
    byGenericUnify coreExp provided provConstrs
                   dependencies depthModMatch depthModNoMatch reasonPart =
      case unify goalType provided of
        -- _a
        -- let b = f _c in _a
        Nothing -> case dependencies of
          [] -> []
          (d:ds) ->
            let vResult = nextVarId s
                vParam = vResult + 1
                vEnd = vParam + 1
                expr = ExpLet vResult (ExpApply coreExp $ ExpHole vParam) (ExpHole var)
                newBinding = (vResult, provided, ds)
                (newScopeId, newScopesRaw) = addScope scopeId $ providedScopes s
                paramGoal = ((vParam, d), scopeId)
                newMainGoal = ((var, goalType), newScopeId)
                --newScopes = scopesAddPBinding newScopeId newBinding newScopesRaw
            in return $ addScopePatternMatch var newScopeId [newBinding] $ State
              (paramGoal:newMainGoal:gr)
              provConstrs
              newScopesRaw
              (functions s)
              (context s)
              (fillExprHole var expr $ expression s)
              vEnd
              (maximum $ maxTVarId s
                       : map largestId dependencies)
              (depth s + depthModNoMatch) -- constant penalty for wild-guessing..
              (Just s)
              ("randomly trying to apply function " ++ show coreExp)
        Just substs -> do
          let contxt = context s
              constrs1 = map (constraintApplySubsts substs)
                       $ constraintGoals s
              constrs2 = map (constraintApplySubsts substs)
                       $ provConstrs
          newConstraints <- maybeToList $ isPossible contxt (constrs1++constrs2)
          let substsTxt   = show substs ++ " unifies " ++ show goalType
                                        ++ " and " ++ show provided
              provableTxt = "constraints (" ++ show (constrs1++constrs2)
                                            ++ ") are provable"
              vBase = nextVarId s
              paramN = length dependencies
              expr = case paramN of
                0 -> coreExp
                n -> foldl ExpApply coreExp (map ExpHole [vBase..vBase+n-1])
              -- newGoals = map (,binds) $ zip [vBase..] dependencies
              newGoals = mkGoals scopeId $ zip [vBase..] dependencies
          return $ State
            (map (goalApplySubst substs) $ newGoals ++ gr)
            newConstraints
            (scopesApplySubsts substs $ providedScopes s)
            (functions s)
            (context s)
            (fillExprHole var expr $ expression s)
            (vBase + paramN)
            (maximum $ maxTVarId s
                     : largestSubstsId substs
                     : map largestId dependencies)
            (depth s + depthModMatch)
            (Just s)
            (reasonPart ++ ", because " ++ substsTxt ++ " and because " ++ provableTxt)

addScopePatternMatch :: Int -> ScopeId -> [VarPBinding] -> State -> State
addScopePatternMatch vid sid bindings state = foldr helper state bindings where
  helper :: VarPBinding -> State -> State
  helper b@(v,vtResult,vtParams) s
    | oldScopes <- providedScopes s,
      defaultRes <- s { providedScopes = scopesAddPBinding sid b oldScopes }
    = if not $ null vtParams then defaultRes
      else case vtResult of
        TypeVar _     -> defaultRes -- dont pattern-match on variables, even if it unifies
        TypeArrow _ _ -> undefined  -- should never happen, given a pbinding..
        TypeForall _ _ -> undefined -- todo when we do RankNTypes
        _ -> fromMaybe defaultRes $ listToMaybe $ do
          MatchBinding matchId matchRs matchParam <- functions s
          let incF = incVarIds (+(1+maxTVarId s))
              resultTypes = map incF matchRs
              inputType = incF matchParam
          substs <- maybeToList $ unifyRight vtResult inputType
          let vBase = nextVarId s
              vEnd = vBase + length resultTypes
              vars = [vBase .. vEnd-1]
              newProvTypes = map (applySubsts substs) resultTypes
              newBinds = map splitBinding $ zip vars $ newProvTypes
              expr = ExpLetMatch matchId vars (ExpVar v) (ExpHole vid)
          return $ addScopePatternMatch vid sid newBinds $ s {
            providedScopes = scopesAddPBinding sid b oldScopes,
            expression = fillExprHole vid expr $ expression s,
            nextVarId = vEnd,
            maxTVarId = maximum (maxTVarId s:map largestId newProvTypes),
            previousState = Just s,
            lastStepReason = "pattern matching on " ++ showVar v
          }


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
