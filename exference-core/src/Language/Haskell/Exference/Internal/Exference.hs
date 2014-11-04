-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}


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
import Language.Haskell.Exference.SearchTree
import Language.Haskell.Exference.Internal.Unify
import Language.Haskell.Exference.Internal.ConstraintSolver
import Language.Haskell.Exference.Internal.ExferenceNode

import qualified Data.PQueue.Prio.Max as Q
import qualified Data.Map as M
import qualified Data.Set as S

import Control.DeepSeq.Generics
import System.Mem.StableName ( StableName, makeStableName )
import System.IO.Unsafe ( unsafePerformIO )

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
import qualified GHC.Conc.Sync

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
  { heuristics_goalVar                :: Float
  , heuristics_goalCons               :: Float
  , heuristics_goalArrow              :: Float
  , heuristics_goalApp                :: Float
  , heuristics_stepProvidedGood       :: Float
  , heuristics_stepProvidedBad        :: Float
  , heuristics_stepEnvGood            :: Float
  , heuristics_stepEnvBad             :: Float
  , heuristics_tempUnusedVarPenalty   :: Float
  , heuristics_tempMultiVarUsePenalty :: Float
  , heuristics_functionGoalTransform  :: Float
  , heuristics_unusedVar              :: Float
  , heuristics_solutionLength         :: Float
  }

data ExferenceInput = ExferenceInput
  { input_goalType    :: HsConstrainedType      -- ^ try to find a expression
                                                -- of this type
  , input_envDict     :: [RatedFunctionBinding] -- ^ the list of functions
                                                -- that may be used
  , input_envClasses  :: StaticClassEnv
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
type ExferenceChunkElement = (BindingUsages, SearchTree, [ExferenceOutputElement])

type RatedNodes = Q.MaxPQueue Float SearchNode

__debug :: Bool
__debug = False

type FindExpressionsState = ( Int    -- number of steps already performed
                            , Float  -- worst rating of state in pqueue
                            , BindingUsages
                            , SearchTreeBuilder (StableName SearchNode)
                            , RatedNodes -- pqueue
                            )

findExpressions :: ExferenceInput
                -> [ExferenceChunkElement]
findExpressions (ExferenceInput rawCType
                                funcs
                                sClassEnv
                                allowUnused
                                maxSteps -- since we output a [[x]],
                                         -- this would not really be
                                         -- necessary anymore. but
                                         -- we also use it for calculating
                                         -- memory limit stuff, and it is
                                         -- not worth the refactor atm.
                                memLimit
                                heuristics) =
  [ (bindingUsages, searchTree, solutions)
  | (bindingUsages, searchTree, stuples) <- resultTuples
  , let solutions = [ (e, ExferenceStats steps compl)
                    | (steps, compl, e) <- stuples
                    ]
  ]
  -- fmap (\(steps, compl, e) -> (e, ExferenceStats steps compl))
  --   <$> resultTuples
  where
    (HsConstrainedType cs t) = ctConstantifyVars rawCType
    rootSearchNode = SearchNode
        [((0, t), 0)]
        []
        initialScopes
        M.empty
        (map splitEnvElement funcs)
        (mkQueryClassEnv sClassEnv cs)
        (ExpHole 0)
        1
        (largestId t)
        0.0
        Nothing
        ""
        Nothing
    initNodeName = unsafePerformIO $ makeStableName $! rootSearchNode
    resultTuples = helper ( 0
                          , 0
                          , emptyBindingUsages
                          , initialSearchTreeBuilder initNodeName (ExpHole 0)
                          , Q.singleton 0.0 rootSearchNode
                          )
    helper :: FindExpressionsState -> [(BindingUsages, SearchTree, [(Int,Float,Expression)])]
    helper (n, worst, bindingUsages, st@(stA, stB), states)
      | Q.null states || n > maxSteps = []
      | ((_,s), restNodes) <- Q.deleteFindMax states =
        let rNodes = stateStep heuristics s
            (potentialSolutions, futures) = partition (null.node_goals) rNodes                                                      
            newBindingUsages = case node_lastStepBinding s of
              Nothing -> bindingUsages
              Just b  -> incBindingUsage b bindingUsages
            out = [ (n, d, e)
                  | solution <- potentialSolutions
                  , null (node_constraintGoals solution)
                  , let unusedVarCount = getUnusedVarCount
                                           (node_varUses solution)
                  , allowUnused || unusedVarCount==0
                  , let e = -- trace (showNodeDevelopment solution) $ 
                            simplifyEta $ simplifyLets $ node_expression solution
                  , let d = node_depth solution
                          + ( heuristics_unusedVar heuristics
                            * fromIntegral unusedVarCount
                            )
                          + ( heuristics_solutionLength heuristics
                            * fromIntegral (length $ show e)
                            )
                  ]
            f :: Float -> Float
            f x | x>900 = 0.0
                | k<-1.111e-3*x = 1 + 2*k**3 - 3*k**2
            ratedNew    = [ ( rateNode heuristics newS + 4.5*f (fromIntegral n)
                            , newS)
                          | newS <- futures ]
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
            newNodes = foldr (uncurry Q.insert) restNodes filteredNew
            newSearchTreeBuilder = if __debug
              then ( [ unsafePerformIO $ do
                         n1 <- makeStableName $! ns
                         n2 <- makeStableName $! s
                         return (n1,n2,node_expression ns)
                     | ns<-rNodes] ++ stA
                   , unsafePerformIO (makeStableName $! s):stB)
              else st
            rest = helper
              ( n+1
              , minimum $ worst:map fst filteredNew
              , newBindingUsages
              , newSearchTreeBuilder
              , newNodes )
        in ( newBindingUsages
           , buildSearchTree newSearchTreeBuilder initNodeName
           , out) : rest

type FindExpressionsParState = ( Int    -- number of calculations currently queued
                               , Int    -- number of steps already performed
                               , Float  -- worst rating of state in pqueue
                               , BindingUsages
                               , SearchTreeBuilder (StableName SearchNode)
                               , RatedNodes -- pqueue
                               )

findExpressionsPar :: ExferenceInput
                   -> (   ListT.ListT IO ExferenceChunkElement
                       -> IO a)
                   -> IO a
findExpressionsPar (ExferenceInput rawCType
                                   funcs
                                   sClassEnv
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
  taskChan   <- newChan :: IO (Chan (Maybe [SearchNode]))
  resultChan <- newChan :: IO (Chan [(Float, SearchNode, SearchNode)])
  let destParallelCount = GHC.Conc.Sync.numCapabilities-1
  let ssCount = 96
  result <- reducer $ do
    let    
      worker = do
        t <- readChan taskChan
        case t of
          Nothing    -> return ()
          Just states -> do
            let g = rateNode heuristics
            let r = [ node_goals s `seq`
                      (rating, newS, s)
                    | s <- states
                    , newS <- stateStep heuristics s
                    , let !rating = g newS
                    ]
            foldr seq () r `seq` writeChan resultChan r
            worker
      controller :: FindExpressionsParState
             -> ListT.ListT IO ( BindingUsages
                               , SearchTreeBuilder (StableName SearchNode)
                               , [(Int,Float,Expression)]
                               )
      controller (nRunning, n, worst, bindingUsages, st@(stA, stB), states) = if
        | n > maxSteps -> mempty
        | n < 768 && not (Q.null states) -> let
            ((_,s), restNodes) = Q.deleteFindMax states
            rNodes = stateStep heuristics s
            (potentialSolutions, futures) = partition (null.node_goals) rNodes
            newBindingUsages = case node_lastStepBinding s of
              Nothing -> bindingUsages
              Just b  -> incBindingUsage b bindingUsages
            out = [ (n, d, e)
                  | solution <- potentialSolutions
                  , null (node_constraintGoals solution)
                  , let unusedVarCount = getUnusedVarCount
                                           (node_varUses solution)
                  , allowUnused || unusedVarCount==0
                  , let e = -- trace (showNodeDevelopment solution) $
                            simplifyEta $ simplifyLets $ node_expression solution
                  , let d = node_depth solution
                          + ( heuristics_unusedVar heuristics
                            * fromIntegral unusedVarCount
                            )
                          + ( heuristics_solutionLength heuristics
                            * fromIntegral (length $ show e)
                            )
                  ]
            f :: Float -> Float
            f x | x>900 = 0.0
                | k<-1.111e-3*x = 1 + 2*k**3 - 3*k**2
            ratedNew    = [ ( rateNode heuristics newS + 4.5*f (fromIntegral n)
                            , newS )
                          | newS <- futures ]
            newNodes = foldr (uncurry Q.insert) restNodes ratedNew
            newSearchTreeBuilder = if __debug
              then ( [ unsafePerformIO $ do
                         n1 <- makeStableName $! ns
                         n2 <- makeStableName $! s
                         return (n1,n2,node_expression ns)
                     | ns<-rNodes] ++ stA
                   , unsafePerformIO (makeStableName $! s):stB)
              else st
            rest = controller
              ( nRunning
              , n+1
              , minimum $ worst:map fst ratedNew
              , newBindingUsages
              , newSearchTreeBuilder
              , newNodes )
            in ListT.cons ( newBindingUsages
                          , newSearchTreeBuilder
                          , out) rest

        | nRunning < 2+destParallelCount && not (Q.null states) -> do
            let ss = map snd $ Q.take ssCount states
                restNodes = Q.drop ssCount states
            lift $ writeChan taskChan (Just ss)
            let calcNew s old = case node_lastStepBinding s of
                  Nothing -> old
                  Just b  -> incBindingUsage b old
            let newBindingUsages = foldr calcNew bindingUsages ss
            let newSearchTreeBuilder = if __debug
                  then ( stA
                       , [ unsafePerformIO (makeStableName $! s)
                         | s <- ss
                         ]++stB
                       )
                  else st
            controller ( nRunning+1
                       , n + length ss
                       , worst
                       , newBindingUsages
                       , newSearchTreeBuilder
                       , restNodes
                       )
        | nRunning==0 -> mempty
        | otherwise -> do
            res <- lift $ readChan resultChan
            let (potentialSolutions, futures) =
                  partition (\(_,x,_) -> null $ node_goals x) res
                out = [ (n, d, e)
                      | (_, solution, _) <- potentialSolutions
                      , null (node_constraintGoals solution)
                      , let unusedVarCount = getUnusedVarCount
                                               (node_varUses solution)
                      , allowUnused || unusedVarCount==0
                      , let e = -- trace (showNodeDevelopment solution) $ 
                                simplifyEta $ simplifyLets $ node_expression solution
                      , let d = node_depth solution
                              + ( heuristics_unusedVar heuristics
                                * fromIntegral unusedVarCount
                                )
                              + ( heuristics_solutionLength heuristics
                                * fromIntegral (length $ show e)
                                )
                      ]
                -- ratedNew    = [ (rateNode heuristics newS, newS) | newS <- futures ]
                qsize = Q.size states
                  -- this cutoff is somewhat arbitrary, and can, theoretically,
                  -- distort the order of the results (i.e.: lead to results being
                  -- omitted).
                filteredNew = if n+qsize > maxSteps
                  then case memLimit of
                    Nothing -> futures
                    Just mmax ->
                      let
                        cutoff = worst * fromIntegral mmax / fromIntegral qsize
                      in
                        filter (\(a,_,_) -> a>cutoff) futures
                  else futures
                newNodes   = foldr (\(r,x,_) -> Q.insert r x) states filteredNew
                newSearchTreeBuilder = if __debug
                  then ( [ unsafePerformIO $ do
                             s <- makeStableName $! newS
                             p <- makeStableName $! oldS
                             return (s,p,node_expression newS)
                         | (_,newS,oldS) <-res] ++ stA
                       , stB)
                  else st
                rest = controller
                  ( nRunning-1
                  , n
                  , minimum $ worst:map (\(r,_,_) -> r) filteredNew
                  , bindingUsages
                  , newSearchTreeBuilder
                  , newNodes )
            ListT.cons (bindingUsages, newSearchTreeBuilder, out) rest
    let 
      (HsConstrainedType cs t) = ctConstantifyVars rawCType
      rootSearchNode = SearchNode
          [((0, t), 0)]
          []
          initialScopes
          M.empty
          (map splitEnvElement funcs)
          (mkQueryClassEnv sClassEnv cs)
          (ExpHole 0)
          1
          (largestId t)
          0.0
          Nothing
          ""
          Nothing
      initNodeName = unsafePerformIO $ makeStableName $! rootSearchNode
    let mapF (a,b,stuples) = ( a
                             , buildSearchTree b initNodeName
                             , [ (e, ExferenceStats steps compl)
                               | (steps, !compl, e) <- stuples]
                             )
    replicateM_ destParallelCount (lift $ forkIO worker)
    mapF <$> controller ( 0
                        , 0
                        , 0
                        , emptyBindingUsages
                        , initialSearchTreeBuilder initNodeName (ExpHole 0)
                        , Q.singleton 0.0 rootSearchNode
                        )
  replicateM_ destParallelCount (writeChan taskChan Nothing)
  return result

ctConstantifyVars :: HsConstrainedType -> HsConstrainedType
ctConstantifyVars (HsConstrainedType a b) =
  HsConstrainedType
    (map (\(HsConstraint c d) -> HsConstraint c $ map tConstantifyVars d) a)
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

rateNode :: ExferenceHeuristicsConfig -> SearchNode -> Float
rateNode h s = 0.0 - rateGoals h (node_goals s) - node_depth s + rateUsage h s
 -- + 0.6 * rateScopes (node_providedScopes s)

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

rateUsage :: ExferenceHeuristicsConfig -> SearchNode -> Float
rateUsage h s = M.foldr f 0.0 vumap
  where
    vumap = node_varUses s
    f :: Int -> Float -> Float
    f 0 x = x - heuristics_tempUnusedVarPenalty h
    f 1 x = x
    f n x = x - fromIntegral (n-1) * heuristics_tempMultiVarUsePenalty h

getUnusedVarCount :: VarUsageMap -> Int
getUnusedVarCount m = length $ filter (==0) $ M.elems m

stateStep :: ExferenceHeuristicsConfig -> SearchNode -> [SearchNode]
stateStep h s = stateStep2 h
              -- $ (\s -> trace (show s ++ " " ++ show (rateNode h s)) s)
              $ s
              -- trace (show (node_depth s) ++ " " ++ show (rateGoals $ node_goals s)
              --                      ++ " " ++ show (rateScopes $ node_providedScopes s)
              --                      ++ " " ++ show (node_expression s)) $

stateStep2 :: ExferenceHeuristicsConfig -> SearchNode -> [SearchNode]
stateStep2 h s
  | node_depth s > 200.0 = []
  | (TypeArrow _ _) <- goalType = arrowStep goalType [] (node_nextVarId s)
  | otherwise = byProvided ++ byFunctionSimple
  where
    (((var, goalType), scopeId):gr) = node_goals s
    arrowStep :: HsType -> [(TVarId, HsType)] -> TVarId -> [SearchNode]
    arrowStep g ts nextId
      | (TypeArrow t1 t2) <- g = arrowStep t2 ((nextId, t1):ts) (nextId+1)
      | otherwise = let
          vEnd = nextId + 1
          (newGoal, newScopeId, newScopes) = addNewScopeGoal scopeId (nextId, g)
                                           $ node_providedScopes s
          newVarUses = M.fromList (map (\(v,_) -> (v,0)) ts) `M.union` node_varUses s
          lambdas = foldr ExpLambda (ExpHole nextId) $ reverse $ map fst ts
          newExpr = fillExprHole var lambdas (node_expression s)
          newBinds = map splitBinding ts
        in return $ addScopePatternMatch nextId newScopeId newBinds $ SearchNode
          (newGoal:gr)
          (node_constraintGoals s)
          newScopes
          newVarUses
          (node_functions s)
          (node_queryClassEnv s)
          newExpr
          vEnd
          (node_maxTVarId s)
          (node_depth s + heuristics_functionGoalTransform h)
          (bool Nothing (Just s) __debug)
          "function goal transform"
          Nothing
    byProvided = do
      (provId, provT, provPs) <- scopeGetAllBindings (node_providedScopes s) scopeId
      byGenericUnify
        (Right provId)
        provT
        (S.toList $ qClassEnv_constraints $ node_queryClassEnv s)
        provPs
        (heuristics_stepProvidedGood h)
        (heuristics_stepProvidedBad h)
        ("inserting given value " ++ show provId ++ "::" ++ show provT)
    byFunctionSimple = do
      SimpleBinding funcId funcRating funcR funcParams funcConstrs <- node_functions s
      let incF = incVarIds (+(1+node_maxTVarId s))
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
                   -> [HsConstraint]
                   -> [HsType]
                   -> Float
                   -> Float
                   -> String
                   -> [SearchNode]
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
          [] -> [] -- we can't (randomly) partially apply a non-function
          (d:ds) ->
            let vResult = node_nextVarId s
                vParam = vResult + 1
                vEnd = vParam + 1
                expr = ExpLet vResult (ExpApply coreExp $ ExpHole vParam) (ExpHole var)
                newBinding = (vResult, provided, ds)
                (newScopeId, newScopesRaw) = addScope scopeId $ node_providedScopes s
                paramGoal = ((vParam, d), scopeId)
                newMainGoal = ((var, goalType), newScopeId)
                --newScopes = scopesAddPBinding newScopeId newBinding newScopesRaw
                newVarUses = M.insert vResult 0 $ case applier of
                  Left _ -> node_varUses s
                  Right i -> M.adjust (+1) i $ node_varUses s
            in return $ addScopePatternMatch var newScopeId [newBinding]
                      $ SearchNode
              (paramGoal:gr++[newMainGoal])
              (node_constraintGoals s ++ provConstrs)
              newScopesRaw
              newVarUses
              (node_functions s)
              (node_queryClassEnv s)
              (fillExprHole var expr $ node_expression s)
              vEnd
              (maximum $ node_maxTVarId s
                       : map largestId dependencies)
              (node_depth s + depthModNoMatch) -- constant penalty for wild-guessing..
              (bool Nothing (Just s) __debug)
              ("randomly trying to apply function " ++ show coreExp)
              bTrace
        Just substs -> do
          let contxt = node_queryClassEnv s
              constrs1 = map (constraintApplySubsts substs)
                       $ node_constraintGoals s
              constrs2 = map (constraintApplySubsts substs)
                       $ provConstrs
          newConstraints <- maybeToList $ isPossible contxt (constrs1++constrs2)
          let substsTxt   = show substs ++ " unifies " ++ show goalType
                                        ++ " and " ++ show provided
              provableTxt = "constraints (" ++ show (constrs1++constrs2)
                                            ++ ") are provable"
              vBase = node_nextVarId s
              paramN = length dependencies
              expr = case paramN of
                0 -> coreExp
                n -> foldl ExpApply coreExp (map ExpHole [vBase..vBase+n-1])
              -- newGoals = map (,binds) $ zip [vBase..] dependencies
              newGoals = mkGoals scopeId $ zip [vBase..] dependencies
              newVarUses = case applier of
                Left _ -> node_varUses s
                Right i -> M.adjust (+1) i $ node_varUses s
          return $ SearchNode
            (map (goalApplySubst substs) $ gr ++ newGoals)
            newConstraints
            (scopesApplySubsts substs $ node_providedScopes s)
            newVarUses
            (node_functions s)
            (node_queryClassEnv s)
            (fillExprHole var expr $ node_expression s)
            (vBase + paramN)
            (maximum $ node_maxTVarId s
                     : largestSubstsId substs
                     : map largestId dependencies)
            (node_depth s + depthModMatch)
            (bool Nothing (Just s) __debug)
            (reasonPart ++ ", because " ++ substsTxt ++ " and because " ++ provableTxt)
            bTrace

addScopePatternMatch :: Int -> ScopeId -> [VarPBinding] -> SearchNode -> SearchNode
addScopePatternMatch vid sid bindings state = foldr helper state bindings where
  helper :: VarPBinding -> SearchNode -> SearchNode
  helper b@(v,vtResult,vtParams) s
    | oldScopes <- node_providedScopes s,
      defaultRes <- s { node_providedScopes = scopesAddPBinding sid b oldScopes }
    = if not $ null vtParams then defaultRes
      else case vtResult of
        TypeVar _     -> defaultRes -- dont pattern-match on variables, even if it unifies
        TypeArrow _ _ -> undefined  -- should never happen, given a pbinding..
        TypeForall _ _ -> undefined -- todo when we do RankNTypes
        _ -> fromMaybe defaultRes $ listToMaybe $ do
          MatchBinding matchId matchRs matchParam <- node_functions s
          let incF = incVarIds (+(1+node_maxTVarId s))
              resultTypes = map incF matchRs
              inputType = incF matchParam
          substs <- maybeToList $ unifyRight vtResult inputType
          let vBase = node_nextVarId s
              vEnd = vBase + length resultTypes
              vars = [vBase .. vEnd-1]
              newProvTypes = map (applySubsts substs) resultTypes
              newBinds = zipWith (curry splitBinding) vars newProvTypes
              expr = ExpLetMatch matchId vars (ExpVar v) (ExpHole vid)
              newVarUses = M.adjust (+1) v (node_varUses s)
                           `M.union` (M.fromList $ zip vars $ repeat 0)
          return $ addScopePatternMatch vid sid newBinds $ s {
            node_providedScopes = scopesAddPBinding sid b oldScopes,
            node_varUses = newVarUses,
            node_expression = fillExprHole vid expr $ node_expression s,
            node_nextVarId = vEnd,
            node_maxTVarId = maximum (node_maxTVarId s:map largestId newProvTypes),
            node_previousNode = bool Nothing (Just s) __debug,
            node_lastStepReason = "pattern matching on " ++ showVar v
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
