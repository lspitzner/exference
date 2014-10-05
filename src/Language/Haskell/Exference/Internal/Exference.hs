-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}

module Language.Haskell.Exference.Internal.Exference
  ( findExpressions
  , findExpressionsPar
  , ExferenceHeuristicsConfig (..)
  , ExferenceInput (..)
  , ExferenceOutputElement
  , ExferenceChunkElement
  )
where



import Language.Haskell.Exference.Type
import Language.Haskell.Exference.Expression
import Language.Haskell.Exference.TypeClasses
import Language.Haskell.Exference.ConstrainedType
import Language.Haskell.Exference.ExferenceStats
import Language.Haskell.Exference.FunctionBinding
import Language.Haskell.Exference.Internal.Unify
import Language.Haskell.Exference.Internal.ConstraintSolver
import Language.Haskell.Exference.Internal.ExferenceState

import qualified Data.PQueue.Prio.Max as Q
import qualified Data.Map as M
import qualified Data.Set as S

import Control.DeepSeq.Generics

import Data.Maybe ( maybeToList, listToMaybe, fromMaybe, catMaybes )
import Control.Arrow ( first, second, (***) )
import Control.Monad ( guard, mzero, replicateM_ )
import Control.Applicative ( (<$>), (<*>) )
import Data.List ( partition, sortBy, groupBy )
import Data.Ord ( comparing )
import Data.Function ( on )
import Data.Bool (bool)
import Data.Monoid ( mempty )
import Control.Monad.Morph ( lift )

import Control.Concurrent.Chan
import Control.Concurrent ( forkIO )

import qualified ListT

-- import Data.DeriveTH
import Debug.Hood.Observe
import Debug.Trace


{-
-- the heuristic input factor constant thingies:
factorGoalVar, factorGoalCons, factorGoalArrow, factorGoalApp,
 factorStepEnvGood, factorStepProvidedGood, factorStepProvidedBad,
 factorStepEnvBad, factorVarUsage, factorFunctionGoalTransform,
 factorUnusedVar :: Float

factorGoalVar               =  4.0
factorGoalCons              =  0.55
factorGoalArrow             =  5.0
factorGoalApp               =  1.9
factorStepProvidedGood      =  0.2
factorStepProvidedBad       =  5.0
factorStepEnvGood           =  6.0
factorStepEnvBad            = 22.0
factorVarUsage              =  8.0
factorFunctionGoalTransform =  0.0
factorUnusedVar             = 20.0
-}

data ExferenceHeuristicsConfig = ExferenceHeuristicsConfig
  { heuristics_goalVar               :: Float
  , heuristics_goalCons              :: Float
  , heuristics_goalArrow             :: Float
  , heuristics_goalApp               :: Float
  , heuristics_stepProvidedGood      :: Float
  , heuristics_stepProvidedBad       :: Float
  , heuristics_stepEnvGood           :: Float
  , heuristics_stepEnvBad            :: Float
  , heuristics_varUsage              :: Float
  , heuristics_functionGoalTransform :: Float
  , heuristics_unusedVar             :: Float
  }

data ExferenceInput = ExferenceInput
  { input_goalType    :: HsConstrainedType      -- ^ try to find a expression
                                                -- of this type
  , input_envDict     :: [RatedFunctionBinding] -- ^ the list of functions
                                                -- that may be used
  , input_envContext  :: StaticContext
  , input_allowUnused :: Bool                   -- ^ if false, forbid solutions
                                                -- where any bind is unused
  , input_maxSteps    :: Int                    -- ^ the maximum number of
                                                -- steps to perform (otherwise
                                                -- would not terminate if
                                                -- there were no (more)
                                                -- solutions)
  , input_memoryLimit :: Maybe Int              -- ^ allows to limit memory
                                                -- usage. no effect if Nothing;
                                                -- for (Just x), memory usage
                                                -- scales with x.
                                                -- Lower memory usage discards
                                                -- states (and, thus, potential
                                                -- solutions).
  , input_heuristicsConfig :: ExferenceHeuristicsConfig
  }

type ExferenceOutputElement = (Expression, ExferenceStats)
type ExferenceChunkElement = (BindingUsages, [ExferenceOutputElement])

type RatedStates = Q.MaxPQueue Float State

__debug :: Bool
__debug = False

type FindExpressionsState = ( Int    -- number of steps already performed
                            , Float  -- worst rating of state in pqueue
                            , BindingUsages
                            , RatedStates -- pqueue
                            )

findExpressions :: ExferenceInput
                -> [ExferenceChunkElement]
findExpressions (ExferenceInput rawCType
                                funcs
                                staticContext
                                allowUnused
                                maxSteps -- since we output a [[x]],
                                         -- this would not really be
                                         -- necessary anymore. but
                                         -- we also use it for calculating
                                         -- memory limit stuff, and it is
                                         -- not worth the refactor atm.
                                memLimit
                                heuristics) =
  [ (bindingUsages, solutions)
  | (bindingUsages, stuples) <- resultTuples
  , let solutions = [ (e, ExferenceStats steps compl)
                    | (steps, compl, e) <- stuples
                    ]
  ]
  -- fmap (\(steps, compl, e) -> (e, ExferenceStats steps compl))
  --   <$> resultTuples
  where
    (HsConstrainedType cs t) = ctConstantifyVars rawCType
    resultTuples = helper (0, 0, emptyBindingUsages,
      Q.singleton 0.0 $ State
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
        Nothing)
    helper :: FindExpressionsState -> [(BindingUsages, [(Int,Float,Expression)])]
    helper (n, worst, bindingUsages, states)
      | Q.null states || n > maxSteps = []
      | ((_,s), restStates) <- Q.deleteFindMax states =
        let (potentialSolutions, futures) = partition (null.state_goals) 
                                                      (stateStep heuristics s)
            newBindingUsages = case state_lastStepBinding s of
              Nothing -> bindingUsages
              Just b  -> incBindingUsage b bindingUsages
            out = [ (n, d, e)
                  | solution <- potentialSolutions
                  , null (state_constraintGoals solution)
                  , let unusedVarCount = getUnusedVarCount
                                           (state_varUses solution)
                  , allowUnused || unusedVarCount==0
                  , let d = state_depth solution
                          + ( heuristics_unusedVar heuristics
                            * fromIntegral unusedVarCount
                            )
                  , let e = -- trace (showStateDevelopment solution) $ 
                            simplifyEta $ simplifyLets $ state_expression solution
                  ]
            ratedNew    = [ (rateState heuristics newS, newS) | newS <- futures ]
            qsize = Q.size states
              -- this cutoff is somewhat arbitrary, and can, theoretically,
              -- distort the order of the results (i.e.: lead to results being
              -- omitted).
            filteredNew = if n+qsize > maxSteps
              then case memLimit of
                Nothing -> ratedNew
                Just mmax ->
                  let
                    cutoff = worst * fromIntegral mmax / fromIntegral qsize
                  in
                    filter ((>cutoff) . fst) ratedNew
              else ratedNew
            newStates   = foldr (uncurry Q.insert) restStates filteredNew
            rest = helper
              ( n+1
              , minimum $ worst:map fst filteredNew
              , newBindingUsages
              , newStates )
        in (newBindingUsages, out) : rest

type FindExpressionsParState = ( Int    -- number of calculations currently queued
                               , Int    -- number of steps already performed
                               , Float  -- worst rating of state in pqueue
                               , BindingUsages
                               , RatedStates -- pqueue
                               )

findExpressionsPar :: ExferenceInput
                   -> (   ListT.ListT IO ExferenceChunkElement
                       -> IO a)
                   -> IO a
findExpressionsPar (ExferenceInput rawCType
                                   funcs
                                   staticContext
                                   allowUnused
                                   maxSteps -- since we output a [[x]],
                                            -- this would not really be
                                            -- necessary anymore. but
                                            -- we also use it for calculating
                                            -- memory limit stuff, and it is
                                            -- not worth the refactor atm.
                                   memLimit
                                   heuristics)
                   reducer
    = do
  taskChan   <- newChan
  resultChan <- newChan
  let destParallelCount = 8
  let ssCount = 16
  result <- reducer $ do
    let    
      worker = do
        t <- readChan taskChan
        case t of
          Nothing    -> return ()
          Just states -> do
            let r = states >>= stateStep heuristics
            let f :: State -> () -> ()
                f (State goals cgoals scops vumap funs cntxt expr nvid mvid d prev reason binding) x =
                        rnf goals
                  `seq` rnf cgoals
                --  `seq` rnf scops
                --  `seq` rnf vumap
                --  `seq` rnf funs
                --  `seq` rnf cntxt
                --  `seq` rnf expr
                --  `seq` rnf nvid
                --  `seq` rnf mvid
                  `seq` rnf d
                --  `seq` rnf prev
                --  `seq` rnf reason
                --  `seq` rnf binding
                  `seq` x
            foldr f () r `seq` writeChan resultChan r
            worker
      controller :: FindExpressionsParState
             -> ListT.ListT IO (BindingUsages, [(Int,Float,Expression)])
      controller (nRunning, n, worst, bindingUsages, states) = if
        | n > maxSteps -> mempty
        | nRunning < 2+destParallelCount && not (Q.null states) -> do
            let ss = map snd $ Q.take ssCount states
                restStates = Q.drop ssCount states
            lift $ writeChan taskChan (Just ss)
            let calcNew s old = case state_lastStepBinding s of
                  Nothing -> old
                  Just b  -> incBindingUsage b old
            let newBindingUsages = foldr calcNew bindingUsages ss
            controller (nRunning+1, n, worst, newBindingUsages, restStates)
        | nRunning==0 -> mempty
        | otherwise -> do
            res <- lift  $ readChan resultChan
            let (potentialSolutions, futures) = partition (null.state_goals) res
                out = [ (n, d, e)
                      | solution <- potentialSolutions
                      , null (state_constraintGoals solution)
                      , let unusedVarCount = getUnusedVarCount
                                               (state_varUses solution)
                      , allowUnused || unusedVarCount==0
                      , let d = state_depth solution
                              + ( heuristics_unusedVar heuristics
                                * fromIntegral unusedVarCount
                                )
                      , let e = -- trace (showStateDevelopment solution) $ 
                                simplifyEta $ simplifyLets $ state_expression solution
                      ]
                ratedNew    = [ (rateState heuristics newS, newS) | newS <- futures ]
                qsize = Q.size states
                  -- this cutoff is somewhat arbitrary, and can, theoretically,
                  -- distort the order of the results (i.e.: lead to results being
                  -- omitted).
                filteredNew = if n+qsize > maxSteps
                  then case memLimit of
                    Nothing -> ratedNew
                    Just mmax ->
                      let
                        cutoff = worst * fromIntegral mmax / fromIntegral qsize
                      in
                        filter ((>cutoff) . fst) ratedNew
                  else ratedNew
                newStates   = foldr (uncurry Q.insert) states filteredNew
                rest = controller
                  ( nRunning-1
                  , n+ssCount
                  , minimum $ worst:map fst filteredNew
                  , bindingUsages
                  , newStates )
            ListT.cons (bindingUsages, out) rest
    let 
      (HsConstrainedType cs t) = ctConstantifyVars rawCType
      initState = (0, 0, 0, emptyBindingUsages,
        Q.singleton 0.0 $ State
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
          Nothing)
    let mapF = second (\stuples -> [ (e, ExferenceStats steps compl)
                                   | (steps, compl, e) <- stuples] )
    replicateM_ destParallelCount (lift $ forkIO worker)
    fmap mapF $ controller initState
  replicateM_ destParallelCount (writeChan taskChan Nothing)
  return result

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

rateState :: ExferenceHeuristicsConfig -> State -> Float
rateState h s = 0.0 - rateGoals h (state_goals s) - state_depth s
 -- + 0.6 * rateScopes (state_providedScopes s)

rateGoals :: ExferenceHeuristicsConfig -> [TGoal] -> Float
rateGoals h = sum . map rateGoal
  where
    rateGoal ((_,t),_) = tComplexity t
    -- TODO: actually measure performance with different values,
    --       use derived values instead of (arbitrarily) chosen ones.
    tComplexity (TypeVar _)       = heuristics_goalVar h
    tComplexity (TypeCons _)      = heuristics_goalCons h
    tComplexity (TypeArrow t1 t2) = heuristics_goalArrow h + tComplexity t1 + tComplexity t2
    tComplexity (TypeApp   t1 t2) = heuristics_goalApp h   + tComplexity t1 + tComplexity t2
    tComplexity (TypeForall _ t1) = tComplexity t1

-- using this rating had bad effect on ordering; not used anymore
{-
rateScopes :: Scopes -> Float
rateScopes (Scopes _ sMap) = M.foldr' f 0.0 sMap
  where
    f (Scope binds _) x = x + fromIntegral (length binds)
-}

getUnusedVarCount :: VarUsageMap -> Int
getUnusedVarCount m = length $ filter (==0) $ M.elems m

stateStep :: ExferenceHeuristicsConfig -> State -> [State]
stateStep h s = --traceShow (s)
              -- trace (show (state_depth s) ++ " " ++ show (rateGoals $ state_goals s)
              --                      ++ " " ++ show (rateScopes $ state_providedScopes s)
              --                      ++ " " ++ show (state_expression s)) $
  stateStep2 h s

stateStep2 :: ExferenceHeuristicsConfig -> State -> [State]
stateStep2 h s
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
          (state_depth s + heuristics_functionGoalTransform h)
          (bool Nothing (Just s) __debug)
          "function goal transform"
          Nothing
    byProvided = do
      (provId, provT, provPs) <- scopeGetAllBindings (state_providedScopes s) scopeId
      let usageFloat, usageRating :: Float
          usageFloat = fromIntegral $ (M.!) (state_varUses s) provId
          usageRating = heuristics_varUsage h * usageFloat * usageFloat
      byGenericUnify
        (Right provId)
        provT
        (S.toList $ dynContext_constraints $ state_context s)
        provPs
        (heuristics_stepProvidedGood h + usageRating)
        (heuristics_stepProvidedBad h + usageRating)
        ("inserting given value " ++ show provId ++ "::" ++ show provT)
    byFunctionSimple = do
      SimpleBinding funcId funcRating funcR funcParams funcConstrs <- state_functions s
      let incF = incVarIds (+(1+state_maxTVarId s))
      byGenericUnify
        (Left funcId)
        (incF funcR)
        (map (constraintMapTypes incF) funcConstrs)
        (map incF funcParams)
        (heuristics_stepEnvGood h + funcRating)
        (heuristics_stepEnvBad h + funcRating)
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
      , bTrace <- case applier of
          Left  x -> Just x
          Right _ -> Nothing
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
            in return $ addScopePatternMatch var newScopeId [newBinding]
                      $ State
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
              (bool Nothing (Just s) __debug)
              ("randomly trying to apply function " ++ show coreExp)
              bTrace
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
            (bool Nothing (Just s) __debug)
            (reasonPart ++ ", because " ++ substsTxt ++ " and because " ++ provableTxt)
            bTrace

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
            state_previousState = bool Nothing (Just s) __debug,
            state_lastStepReason = "pattern matching on " ++ showVar v
          }


splitEnvElement :: RatedFunctionBinding -> FuncDictElem
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
