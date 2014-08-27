-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}

module Infression
  ( findExpressions
  , findOneExpression
  , findSortNExpressions
  , findBestNExpressions
  , findFirstBestExpressions
  , takeFindSortNExpressions
  , ExferenceInput ( ExferenceInput )
  , ExferenceOutputElement
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
import Data.List ( partition, sortBy, groupBy )
import Data.Ord ( comparing )
import Data.Function ( on )

-- import Data.DeriveTH
import Debug.Hood.Observe
import Debug.Trace



-- the heuristic input factor constant thingies:
factorGoalVar, factorGoalCons, factorGoalArrow, factorGoalApp,
 factorStepEnvGood, factorStepProvidedGood, factorStepProvidedBad,
 factorStepEnvBad, factorVarUsage, factorFunctionGoalTransform,
 factorUnusedVar :: Float

factorGoalVar   = 4.0
factorGoalCons  = 0.55
factorGoalArrow = 5.0
factorGoalApp   = 1.9
factorStepProvidedGood = 0.2
factorStepProvidedBad  = 5.0
factorStepEnvGood = 6.0
factorStepEnvBad  = 22.0
factorVarUsage = 8.0
factorFunctionGoalTransform = 0.0
factorUnusedVar = 20.0

data ExferenceInput = ExferenceInput
  { goalType :: HsConstrainedType
  , envFunctions :: [(String, Float, HsConstrainedType)]
  , envContext :: StaticContext
  , allowUnused :: Bool
  }

type ExferenceOutputElement = (Expression, InfressionStats)

type RatedStates = Q.MaxPQueue Float State

-- returns the first found solution (not necessarily the best overall)
findOneExpression :: ExferenceInput
                  -> Maybe ExferenceOutputElement
findOneExpression input = listToMaybe $ findExpressions input

-- calculates at most n solutions, sorts by rating, returns the first m
takeFindSortNExpressions :: Int
                         -> Int
                         -> ExferenceInput
                         -> [ExferenceOutputElement]
takeFindSortNExpressions m n input =
  take m $ findSortNExpressions n input

-- calculates at most n solutions, and returns them sorted by their rating
findSortNExpressions :: Int
                     -> ExferenceInput
                     -> [ExferenceOutputElement]
findSortNExpressions n input = sortBy (comparing g) $ take n $ r
  where
    r = findExpressions input
    g (_,InfressionStats _ f) = f

-- returns the first expressions with the best rating.
-- best explained on examples:
--   []      -> []
--   [2,5,5] -> [2]
--   [3,3,3,4,4,5,6,7] -> [3,3,3]
--   [2,5,2] -> [2] -- will not look past worse ratings
--   [4,3,2,2,2,3] -> [2,2,2] -- if directly next is better, switch to that
findFirstBestExpressions :: ExferenceInput
                         -> [ExferenceOutputElement]
findFirstBestExpressions input
  | r <- findExpressions input
  , f <- head . groupBy ((>=) `on` infression_complexityRating.snd)
  = case r of
    [] -> []
    _  -> f $ reverse $ f $ r

-- like findSortNExpressions, but retains only the best rating
findBestNExpressions :: Int
                     -> ExferenceInput
                     -> [ExferenceOutputElement]
findBestNExpressions n input
  | r <- findSortNExpressions n input
  = case r of
    [] -> []
    _  -> head $ groupBy ((>=) `on` infression_complexityRating.snd) $ r

findExpressions :: ExferenceInput
                -> [ExferenceOutputElement]
findExpressions (ExferenceInput rawCType funcs staticContext allowUnused) =
  [(e, InfressionStats steps compl) | (steps, compl, e) <- r]
  where
    (HsConstrainedType cs t) = ctConstantifyVars rawCType
    r = helper 0 $ Q.singleton 0.0 $ State
      [((0, t), 0)]
      []
      initialScopes
      M.empty
      (map splitEnvElement funcs)
      (mkDynContext staticContext cs)
      (ExpHole 0)
      1
      (largestId t)
      0.0
      Nothing
      ""
    helper :: Int -> RatedStates -> [(Int,Float,Expression)]
    helper n states
      | Q.null states || n > 32768 = []
      | ((_,s), restStates) <- Q.deleteFindMax states =
        let (potentialSolutions, futures) = partition (null.state_goals) 
                                                      (stateStep s)
            out = [ (n, d, e)
                  | solution <- potentialSolutions
                  , null (state_constraintGoals solution)
                  , let unusedVarCount = getUnusedVarCount
                                           (state_varUses solution)
                  , allowUnused || unusedVarCount==0
                  , let d = state_depth solution
                          + factorUnusedVar*(fromIntegral unusedVarCount)
                  , let e = -- trace (showStateDevelopment solution) $ 
                            simplifyLets $ state_expression solution
                  ]
            rest = helper (n+1) $ foldr 
                       (uncurry Q.insert)
                       restStates
                       [ (r, newS) | newS <- futures
                                   , let r = rateState newS ]
        in out ++ rest


ctConstantifyVars :: HsConstrainedType -> HsConstrainedType
ctConstantifyVars (HsConstrainedType a b) =
  HsConstrainedType
    (map (\(Constraint c d) -> Constraint c $ map tConstantifyVars d) a)
    (tConstantifyVars b)

tConstantifyVars :: HsType -> HsType
tConstantifyVars (TypeVar i)        = TypeCons $ "EXF" ++ showVar i
tConstantifyVars c@(TypeCons _)     = c
tConstantifyVars (TypeArrow t1 t2)  = TypeArrow
                                       (tConstantifyVars t1)
                                       (tConstantifyVars t2)
tConstantifyVars (TypeApp t1 t2)    = TypeApp
                                       (tConstantifyVars t1)
                                       (tConstantifyVars t2)
tConstantifyVars f@(TypeForall _ _) = f

rateState :: State -> Float
rateState s = 0.0 - rateGoals (state_goals s) - state_depth s
 -- + 0.6 * rateScopes (state_providedScopes s)

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

rateScopes :: Scopes -> Float
rateScopes (Scopes _ sMap) = M.foldr' f 0.0 sMap
  where
    f (Scope binds _) x = x + fromIntegral (length binds)

getUnusedVarCount :: VarUsageMap -> Int
getUnusedVarCount m = length $ filter (==0) $ M.elems m

stateStep :: State -> [State]
stateStep s = --traceShow (state_expression s)
              -- trace (show (state_depth s) ++ " " ++ show (rateGoals $ state_goals s)
              --                      ++ " " ++ show (rateScopes $ state_providedScopes s)
              --                      ++ " " ++ show (state_expression s)) $
  stateStep2 s

stateStep2 :: State -> [State]
stateStep2 s
  | state_depth s > 200.0 = []
  | (TypeArrow _ _) <- goalType = arrowStep goalType [] (state_nextVarId s)
  | otherwise = byProvided ++ byFunctionSimple
  where
    (((var, goalType), scopeId):gr) = state_goals s
    arrowStep :: HsType -> [(TVarId, HsType)] -> TVarId -> [State]
    arrowStep g ts nextId
      | (TypeArrow t1 t2) <- g = arrowStep t2 ((nextId, t1):ts) (nextId+1)
      | otherwise = let
          vEnd = nextId + 1
          (newGoal, newScopeId, newScopes) = addNewScopeGoal scopeId (nextId, g)
                                           $ state_providedScopes s
          newVarUses = M.fromList (map (\(v,_) -> (v,0)) ts) `M.union` state_varUses s
          lambdas = foldr ExpLambda (ExpHole nextId) $ reverse $ map fst ts
          newExpr = fillExprHole var lambdas (state_expression s)
          newBinds = map splitBinding ts
        in return $ addScopePatternMatch nextId newScopeId newBinds $ State
          (newGoal:gr)
          (state_constraintGoals s)
          newScopes
          newVarUses
          (state_functions s)
          (state_context s)
          newExpr
          vEnd
          (state_maxTVarId s)
          (state_depth s + factorFunctionGoalTransform)
          (Just s)
          "function goal transform"
    byProvided = do
      (provId, provT, provPs) <- scopeGetAllBindings (state_providedScopes s) scopeId
      let usageFloat, usageRating :: Float
          usageFloat = fromIntegral $ (M.!) (state_varUses s) provId
          usageRating = factorVarUsage * usageFloat * usageFloat
      byGenericUnify
        (Right provId)
        provT
        (S.toList $ dynContext_constraints $ state_context s)
        provPs
        (factorStepProvidedGood + usageRating)
        (factorStepProvidedBad + usageRating)
        ("inserting given value " ++ show provId ++ "::" ++ show provT)
    byFunctionSimple = do
      SimpleBinding funcId funcRating funcR funcParams funcConstrs <- state_functions s
      let incF = incVarIds (+(1+state_maxTVarId s))
      byGenericUnify
        (Left funcId)
        (incF funcR)
        (map (constraintMapTypes incF) funcConstrs)
        (map incF funcParams)
        (factorStepEnvGood + funcRating)
        (factorStepEnvBad + funcRating)
        ("applying function " ++ show funcId)
    byGenericUnify :: Either String TVarId
                   -> HsType
                   -> [Constraint]
                   -> [HsType]
                   -> Float
                   -> Float
                   -> String
                   -> [State]
    byGenericUnify applier provided provConstrs
                   dependencies depthModMatch depthModNoMatch reasonPart
      | coreExp <- either ExpLit ExpVar applier
      = case unify goalType provided of
        -- _a
        -- let b = f _c in _a
        Nothing -> case dependencies of
          [] -> []
          (d:ds) ->
            let vResult = state_nextVarId s
                vParam = vResult + 1
                vEnd = vParam + 1
                expr = ExpLet vResult (ExpApply coreExp $ ExpHole vParam) (ExpHole var)
                newBinding = (vResult, provided, ds)
                (newScopeId, newScopesRaw) = addScope scopeId $ state_providedScopes s
                paramGoal = ((vParam, d), scopeId)
                newMainGoal = ((var, goalType), newScopeId)
                --newScopes = scopesAddPBinding newScopeId newBinding newScopesRaw
                newVarUses = M.insert vResult 0 $ case applier of
                  Left _ -> state_varUses s
                  Right i -> M.adjust (+1) i $ state_varUses s
            in return $ addScopePatternMatch var newScopeId [newBinding] $ State
              (paramGoal:newMainGoal:gr)
              (state_constraintGoals s ++ provConstrs)
              newScopesRaw
              newVarUses
              (state_functions s)
              (state_context s)
              (fillExprHole var expr $ state_expression s)
              vEnd
              (maximum $ state_maxTVarId s
                       : map largestId dependencies)
              (state_depth s + depthModNoMatch) -- constant penalty for wild-guessing..
              (Just s)
              ("randomly trying to apply function " ++ show coreExp)
        Just substs -> do
          let contxt = state_context s
              constrs1 = map (constraintApplySubsts substs)
                       $ state_constraintGoals s
              constrs2 = map (constraintApplySubsts substs)
                       $ provConstrs
          newConstraints <- maybeToList $ isPossible contxt (constrs1++constrs2)
          let substsTxt   = show substs ++ " unifies " ++ show goalType
                                        ++ " and " ++ show provided
              provableTxt = "constraints (" ++ show (constrs1++constrs2)
                                            ++ ") are provable"
              vBase = state_nextVarId s
              paramN = length dependencies
              expr = case paramN of
                0 -> coreExp
                n -> foldl ExpApply coreExp (map ExpHole [vBase..vBase+n-1])
              -- newGoals = map (,binds) $ zip [vBase..] dependencies
              newGoals = mkGoals scopeId $ zip [vBase..] dependencies
              newVarUses = case applier of
                Left _ -> state_varUses s
                Right i -> M.adjust (+1) i $ state_varUses s
          return $ State
            (map (goalApplySubst substs) $ newGoals ++ gr)
            newConstraints
            (scopesApplySubsts substs $ state_providedScopes s)
            newVarUses
            (state_functions s)
            (state_context s)
            (fillExprHole var expr $ state_expression s)
            (vBase + paramN)
            (maximum $ state_maxTVarId s
                     : largestSubstsId substs
                     : map largestId dependencies)
            (state_depth s + depthModMatch)
            (Just s)
            (reasonPart ++ ", because " ++ substsTxt ++ " and because " ++ provableTxt)

addScopePatternMatch :: Int -> ScopeId -> [VarPBinding] -> State -> State
addScopePatternMatch vid sid bindings state = foldr helper state bindings where
  helper :: VarPBinding -> State -> State
  helper b@(v,vtResult,vtParams) s
    | oldScopes <- state_providedScopes s,
      defaultRes <- s { state_providedScopes = scopesAddPBinding sid b oldScopes }
    = if not $ null vtParams then defaultRes
      else case vtResult of
        TypeVar _     -> defaultRes -- dont pattern-match on variables, even if it unifies
        TypeArrow _ _ -> undefined  -- should never happen, given a pbinding..
        TypeForall _ _ -> undefined -- todo when we do RankNTypes
        _ -> fromMaybe defaultRes $ listToMaybe $ do
          MatchBinding matchId matchRs matchParam <- state_functions s
          let incF = incVarIds (+(1+state_maxTVarId s))
              resultTypes = map incF matchRs
              inputType = incF matchParam
          substs <- maybeToList $ unifyRight vtResult inputType
          let vBase = state_nextVarId s
              vEnd = vBase + length resultTypes
              vars = [vBase .. vEnd-1]
              newProvTypes = map (applySubsts substs) resultTypes
              newBinds = map splitBinding $ zip vars $ newProvTypes
              expr = ExpLetMatch matchId vars (ExpVar v) (ExpHole vid)
              newVarUses = M.adjust (+1) v (state_varUses s)
                           `M.union` (M.fromList $ zip vars $ repeat 0)
          return $ addScopePatternMatch vid sid newBinds $ s {
            state_providedScopes = scopesAddPBinding sid b oldScopes,
            state_varUses = newVarUses,
            state_expression = fillExprHole vid expr $ state_expression s,
            state_nextVarId = vEnd,
            state_maxTVarId = maximum (state_maxTVarId s:map largestId newProvTypes),
            state_previousState = Just s,
            state_lastStepReason = "pattern matching on " ++ showVar v
          }


splitEnvElement :: (String, Float, HsConstrainedType)
                  -> FuncBinding
                  --  name resultType paramTypes constraints
splitEnvElement (a,r,HsConstrainedType constrs b) =
  case f b of
    (Left  t,  ps) -> SimpleBinding a r t ps constrs
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
