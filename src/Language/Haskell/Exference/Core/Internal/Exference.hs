-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}


module Language.Haskell.Exference.Core.Internal.Exference
  ( findExpressions
  , ExferenceHeuristicsConfig (..)
  , ExferenceInput (..)
  , ExferenceOutputElement
  , ExferenceChunkElement
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.ExferenceStats
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.SearchTree
import Language.Haskell.Exference.Core.Internal.Unify
import Language.Haskell.Exference.Core.Internal.ConstraintSolver
import Language.Haskell.Exference.Core.Internal.ExferenceNode
import Language.Haskell.Exference.Core.Internal.ExferenceNodeBuilder

import Control.Monad.Trans.MultiState

import qualified Data.PQueue.Prio.Max as Q
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Sequence as Seq

import Control.DeepSeq.Generics
import System.Mem.StableName ( StableName, makeStableName )
import System.IO.Unsafe ( unsafePerformIO )

import Data.Maybe ( maybeToList, listToMaybe, fromMaybe, catMaybes )
import Control.Arrow ( first, second, (***) )
import Control.Monad ( when, unless, guard, mzero, replicateM
                     , replicateM_, forM, join, forM_ )
import Control.Applicative ( (<$>), (<*>) )
import Data.List ( partition, sortBy, groupBy )
import Data.Ord ( comparing )
import Data.Function ( on )
import Data.Monoid ( mempty, First(First), getFirst, mconcat )
import Data.Foldable ( foldMap, sum )
import Control.Monad.Morph ( lift )
import Data.Typeable ( Typeable )

-- import Control.Concurrent.Chan
import Control.Concurrent ( forkIO )
import qualified GHC.Conc.Sync

import qualified ListT

import Data.Data ( Data )

-- import Data.DeriveTH
import Debug.Hood.Observe
import Debug.Trace

import Prelude hiding ( sum )



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
  deriving (Show, Data, Typeable)

data ExferenceInput = ExferenceInput
  { input_goalType    :: HsType                 -- ^ try to find a expression
                                                -- of this type
  , input_envFuncs    :: [FunctionBinding]      -- ^ the list of functions
                                                -- that may be used
  , input_envDeconsS  :: [DeconstructorBinding] -- ^ the list of deconstructors
                                                -- that may be used for pattern
                                                -- matching
  , input_envClasses  :: StaticClassEnv
  , input_allowUnused :: Bool                   -- ^ if false, forbid solutions
                                                -- where any bind is unused
  , input_allowConstraints :: Bool              -- ^ if true, allow solutions
                                                -- that have unproven
                                                -- constraints remaining.
  , input_allowConstraintsStopStep :: Int       -- ^ stop ignoring
                                                -- tc-constraints after this
                                                -- step to have some chance to
                                                -- find some solution.
  , input_multiPM     :: Bool                   -- ^ pattern match on
                                                -- multi-constructor data types
                                                -- if true. serverly increases
                                                -- search space (decreases
                                                -- performance).
  , input_qNameIndex  :: QNameIndex             -- ^ the qNameIndex for
                                                -- looking up the ids in
                                                -- expressions.
  , input_maxSteps    :: Int                    -- ^ the maximum number of
                                                -- steps to perform (otherwise
                                                -- would not terminate if
                                                -- there were no (more)
                                                -- solutions).
  , input_memoryLimit :: Maybe Int              -- ^ allows to limit memory
                                                -- usage. no effect if Nothing;
                                                -- for (Just x), memory usage
                                                -- scales with x.
                                                -- Lower memory usage discards
                                                -- states (and, thus, potential
                                                -- solutions).
  , input_heuristicsConfig :: ExferenceHeuristicsConfig
  }
  deriving (Show, Data, Typeable)

type ExferenceOutputElement = (Expression, [HsConstraint], ExferenceStats)
type ExferenceChunkElement = (BindingUsages, SearchTree, [ExferenceOutputElement])

type RatedNodes = Q.MaxPQueue Float SearchNode

type FindExpressionsState = ( Int    -- number of steps already performed
                            , Float  -- worst rating of state in pqueue
                            , BindingUsages
                            , SearchTreeBuilder (StableName SearchNode)
                            , RatedNodes -- pqueue
                            )

findExpressions :: ExferenceInput
                -> [ExferenceChunkElement]
findExpressions (ExferenceInput rawType
                                funcs
                                deconss
                                sClassEnv
                                allowUnused
                                allowConstraints
                                allowConstraintsStopStep
                                multiPM
                                qNameIndex
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
  , let solutions = [ (e, constrs, ExferenceStats steps compl fsize)
                    | (steps, compl, e, constrs, fsize) <- stuples
                    ]
  ]
  -- fmap (\(steps, compl, e) -> (e, ExferenceStats steps compl))
  --   <$> resultTuples
  where
    t = forallify rawType
    rootSearchNode = SearchNode
        (Seq.singleton (VarBinding 0 t, 0))
        []
        initialScopes
        IntMap.empty
        (V.fromList funcs) -- TODO: lift this further up?
        deconss
        (mkQueryClassEnv sClassEnv [])
        (ExpHole 0)
        1 -- TODO: change to 0?
        (largestId t)
        0
        0.0
#if LINK_NODES
        Nothing
#endif
        ""
        Nothing
    initNodeName = unsafePerformIO $ makeStableName $! rootSearchNode
    resultTuples = helper ( 0
                          , 0
                          , emptyBindingUsages
                          , initialSearchTreeBuilder initNodeName (ExpHole 0)
                          , Q.singleton 0.0 rootSearchNode
                          )
    helper :: FindExpressionsState
           -> [( BindingUsages
               , SearchTree
               , [(Int,Float,Expression,[HsConstraint],Int)]
               )]
    helper (n, worst, bindingUsages, st, states)
      | Q.null states || n > maxSteps = []
      | ((_,s), restNodes) <- Q.deleteFindMax states =
        let rNodes = stateStep multiPM
                               (allowConstraints && n<allowConstraintsStopStep)
                               qNameIndex
                               heuristics
                               s
            (potentialSolutions, futures) = partition (Seq.null . node_goals) rNodes                                                      
            newBindingUsages = case node_lastStepBinding s of
              Nothing -> bindingUsages
              Just b  -> incBindingUsage b bindingUsages
            out = [ (n, d, e, remainingConstraints, qsize)
                  | solution <- potentialSolutions
                  , let contxt = node_queryClassEnv solution
                  , remainingConstraints <- maybeToList
                                          $ filterUnresolved contxt
                                          $ node_constraintGoals solution
                  , allowConstraints || null remainingConstraints
                  , let unusedVarCount = getUnusedVarCount
                                           (node_varUses solution)
                  , allowUnused || unusedVarCount==0
                  , let e = -- trace (showNodeDevelopment solution) $ 
                            node_expression solution
                  , let d = node_depth solution
                          + ( heuristics_unusedVar heuristics
                            * fromIntegral unusedVarCount
                            )
                          -- + ( heuristics_solutionLength heuristics
                          --   * fromIntegral (length $ show e)
                          --   )
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
            newSearchTreeBuilder =
#if BUILD_SEARCH_TREE
              let (stA,stB)=st in
                    ( [ unsafePerformIO $ do
                          n1 <- makeStableName $! ns
                          n2 <- makeStableName $! s
                          return (n1,n2,node_expression ns)
                      | ns<-rNodes] ++ stA
                    , unsafePerformIO (makeStableName $! s):stB)
#else
              st
#endif
            rest = helper
              ( n+1
              , minimum $ worst:map fst filteredNew
              , newBindingUsages
              , newSearchTreeBuilder
              , newNodes )
        in ( newBindingUsages
           , buildSearchTree newSearchTreeBuilder initNodeName
           , out
           ) : rest

rateNode :: ExferenceHeuristicsConfig -> SearchNode -> Float
rateNode h s = 0.0 - rateGoals h (node_goals s) - node_depth s + rateUsage h s
 -- + 0.6 * rateScopes (node_providedScopes s)

rateGoals :: ExferenceHeuristicsConfig -> Seq.Seq TGoal -> Float
rateGoals h = sum . fmap rateGoal
  where
    rateGoal (VarBinding _ t,_) = tComplexity t
    -- TODO: actually measure performance with different values,
    --       use derived values instead of (arbitrarily) chosen ones.
    tComplexity (TypeVar _)         = heuristics_goalVar h
    tComplexity (TypeConstant _)    = heuristics_goalCons h -- TODO different heuristic?
    tComplexity (TypeCons _)        = heuristics_goalCons h
    tComplexity (TypeArrow t1 t2)   = heuristics_goalArrow h + tComplexity t1 + tComplexity t2
    tComplexity (TypeApp   t1 t2)   = heuristics_goalApp h   + tComplexity t1 + tComplexity t2
    tComplexity (TypeForall _ _ t1) = tComplexity t1

-- using this rating had bad effect on ordering; not used anymore
{-
rateScopes :: Scopes -> Float
rateScopes (Scopes _ sMap) = M.foldr' f 0.0 sMap
  where
    f (Scope binds _) x = x + fromIntegral (length binds)
-}

rateUsage :: ExferenceHeuristicsConfig -> SearchNode -> Float
rateUsage h s = IntMap.foldr f 0.0 $ node_varUses s
  where
    f :: Int -> Float -> Float
    f 0 x = x - heuristics_tempUnusedVarPenalty h
    f 1 x = x
    f n x = x - fromIntegral (n-1) * heuristics_tempMultiVarUsePenalty h

getUnusedVarCount :: VarUsageMap -> Int
getUnusedVarCount m = length $ filter (==0) $ IntMap.elems m

stateStep :: Bool
          -> Bool
          -> QNameIndex
          -> ExferenceHeuristicsConfig
          -> SearchNode
          -> [SearchNode]
stateStep multiPM allowConstrs qNameIndex h s
  = stateStep2 multiPM allowConstrs qNameIndex h
              -- $ (\_ -> trace (showSearchNode' qNameIndex s ++ " " ++ show (rateNode h s)) s)
              $ s
              -- trace (show (node_depth s) ++ " " ++ show (rateGoals $ node_goals s)
              --                      ++ " " ++ show (rateScopes $ node_providedScopes s)
              --                      ++ " " ++ show (node_expression s)) $

stateStep2 :: Bool
           -> Bool
           -> QNameIndex
           -> ExferenceHeuristicsConfig
           -> SearchNode
           -> [SearchNode]
stateStep2 multiPM allowConstrs qNameIndex h s
  | node_depth s > 200.0 = []
  | (TypeArrow _ _) <- goalType = [ modifyNodeBy s' $ arrowStep goalType [] ]
  | (TypeForall is cs t) <- goalType = [ modifyNodeBy s' $ forallStep is cs t ]
  | otherwise = byProvided ++ byFunctionSimple
  where
    ((VarBinding var goalType, scopeId) Seq.:< gr) = Seq.viewl $ node_goals s
    s' = s { node_goals = gr }
    arrowStep :: HsType -> [VarBinding] -> SearchNodeBuilder ()
    arrowStep g ts
      | (TypeArrow t1 t2) <- g = do
          nextId <- case t1 of
            TypeArrow {} -> builderAllocVarF
            _            -> builderAllocVar
          arrowStep t2 ((VarBinding nextId t1):ts)
      | otherwise = do
          nextId <- builderAllocHole
          newScopeId <- builderAddScope scopeId
          builderFillExprHole var
            $ foldr ExpLambda (ExpHole nextId) $ reverse $ map (\(VarBinding v _) -> v) ts
          builderAddDepth (heuristics_functionGoalTransform h)
          builderSetReason "function goal transform"
          builderSetLastStepBinding Nothing
          mapM_ builderAppendGoal =<< ( addScopePatternMatch multiPM g nextId newScopeId
                                      $ map splitBinding
                                      $ reverse
                                      $ ts
                                      )
    forallStep :: [TVarId] -> [HsConstraint] -> HsType -> SearchNodeBuilder ()
    forallStep vs cs t = do
      dataIds <- mapM (const builderAllocNVar) vs
      builderAddDepth (heuristics_functionGoalTransform h) -- TODO: different heuristic?
      builderSetReason "forall-type goal transformation"
      builderSetLastStepBinding Nothing
      let substs = IntMap.fromList $ zip vs $ TypeConstant <$> dataIds
      builderPrependGoal (VarBinding var (applySubsts substs t), scopeId)
      builderAddGivenConstraints $ constraintApplySubsts substs <$> cs
      return ()
    byProvided = do
      (provId, provT, provPs, forallTypes, constraints) <- scopeGetAllBindings (node_providedScopes s) scopeId
      let incF = incVarIds (+(1+node_maxTVarId s))
      let ss = IntMap.fromList $ zip forallTypes (incF . TypeVar <$> forallTypes)
      let provType = applySubsts ss provT
      let provConstrs = S.toList $           qClassEnv_constraints (node_queryClassEnv s)
                                   `S.union` S.fromList (constraintApplySubsts ss <$> constraints)
      byGenericUnify
        (Right provId)
        (provType)
        (provConstrs)
        (applySubsts ss <$> provPs)
        (heuristics_stepProvidedGood h)
        (heuristics_stepProvidedBad h)
        ("inserting given value " ++ show provId ++ "::" ++ show provT)
        (unify goalType provType)
    byFunctionSimple = do
      (funcR, funcId, funcRating, funcConstrs, funcParams) <- V.toList $ node_functions s
      let offset = 1+node_maxTVarId s
      let incF = incVarIds (+offset)
      let provType = incF funcR
      byGenericUnify
        (Left funcId)
        (provType)
        (map (constraintMapTypes incF) funcConstrs)
        (map incF funcParams)
        (heuristics_stepEnvGood h + funcRating)
        (heuristics_stepEnvBad h + funcRating)
        ("applying function " ++ show funcId)
        (unifyOffset goalType (HsTypeOffset funcR offset))
    byGenericUnify :: Either QNameId TVarId
                   -> HsType
                   -> [HsConstraint]
                   -> [HsType]
                   -> Float
                   -> Float
                   -> String
                   -> Maybe (Substs, Substs)
                   -> [SearchNode]
    byGenericUnify applier
                   provided
                   provConstrs
                   dependencies
                   depthModMatch
                   depthModNoMatch
                   reasonPart
                   unifyResult
      | coreExp <- either ExpName ExpVar applier
      , bTrace <- case applier of
          Left  x -> Just (show x)
          Right _ -> Nothing
      = case
          -- (\r -> case r of Nothing -> r; Just jr -> traceShow (goalType, provided, jr) r) $
          unifyResult of
        Nothing -> case dependencies of
          [] -> [] -- we can't (randomly) partially apply a non-function
          (d:ds) -> return $ modifyNodeBy s' $ do
            vResult <- builderAllocVar
            vParam  <- builderAllocHole
            builderFillExprHole var $ ExpLet
                                        vResult
                                        (ExpApply coreExp $ ExpHole vParam)
                                        (ExpHole var)
            builderPrependGoal (VarBinding vParam d, scopeId)
            newScopeId <- builderAddScope scopeId
            builderAddConstraintGoals provConstrs
            case applier of
                Left _ -> return ()
                Right i -> builderAddVarUsage i
            builderFixMaxTVarId $ maximum $ map largestId dependencies
            builderAddDepth depthModNoMatch
            builderSetReason $ "randomly trying to apply function "
                              ++ showExpressionPure qNameIndex coreExp
            mapM_ builderAppendGoal =<< addScopePatternMatch
              multiPM
              goalType
              var
              newScopeId
              (let (r,ps,fs,cs) = splitArrowResultParams provided
                in [(vResult, r, ds++ps, fs, cs)])
        Just (leftSS, rightSS) -> do
          let allSS = IntMap.union leftSS rightSS
          let substs = case applier of
                Left _  -> leftSS
                Right _ -> allSS
              contxt = node_queryClassEnv s
              constrs1 = map (constraintApplySubsts' substs)
                       $ node_constraintGoals s
              constrs2 = map (constraintApplySubsts rightSS)
                       $ provConstrs
          newConstraints <- if allowConstrs
            then [map snd constrs1 ++ constrs2]
            else if any fst constrs1
              then maybeToList $ isPossible contxt (map snd constrs1 ++ constrs2)
              else (map snd constrs1 ++) <$> maybeToList (isPossible contxt constrs2)
          return $ modifyNodeBy s' $ do
            let paramN = length dependencies
            vars <- replicateM paramN builderAllocHole
            let newGoals = mkGoals scopeId $ zipWith VarBinding vars dependencies
            case applier of
              Left _  ->
                forM_ newGoals (builderAppendGoal . goalApplySubst rightSS)
              Right _ -> 
                forM_ newGoals builderAppendGoal
            builderApplySubst substs
            builderFillExprHole var $ case paramN of
              0 -> coreExp
              _ -> foldl ExpApply coreExp (map ExpHole vars)
            case applier of
              Left _ -> return ()
              Right i -> builderAddVarUsage i
            builderSetConstraints newConstraints
            builderFixMaxTVarId $ maximum
                                $ largestSubstsId leftSS
                                  : map largestId dependencies
            builderAddDepth depthModMatch
            let substsTxt   = show (IntMap.union leftSS rightSS)
                              ++ " unifies "
                              ++ show goalType
                              ++ " and "
                              ++ show provided
            let provableTxt = "constraints (" ++ show (map snd constrs1++constrs2)
                                              ++ ") are provable"
            builderSetReason $ reasonPart ++ ", because " ++ substsTxt
                              ++ " and because " ++ provableTxt
            builderSetLastStepBinding bTrace

{-# INLINE addScopePatternMatch #-}
addScopePatternMatch :: Bool -- should p-m on anything but newtypes?
                     -> HsType -- the current goal (should be returned in one
                               --  form or another)
                     -> Int    -- goal id (hole id)
                     -> ScopeId -- scope for this goal
                     -> [VarPBinding]
                     -> SearchNodeBuilder [TGoal]
addScopePatternMatch multiPM goalType vid sid bindings = case bindings of
  []                                    -> return [(VarBinding vid goalType, sid)]
  (b@(v,vtResult,vtParams,_,_):bindingRest) -> do
    offset <- builderGetTVarOffset
    let incF = incVarIds (+offset)
    builderAddPBinding sid b
    let defaultHandleRest = addScopePatternMatch multiPM goalType vid sid bindingRest
    case vtResult of
      TypeVar {}    -> defaultHandleRest -- dont pattern-match on variables, even if it unifies
      TypeArrow {}  -> error $ "addScopePatternMatch: TypeArrow: " ++ show vtResult  -- should never happen, given a pbinding..
      TypeForall {} -> error
                       $ "addScopePatternMatch: TypeForall (RankNTypes not yet implemented)" -- todo when we do RankNTypes
                       ++ show vtResult
      _ | not $ null vtParams -> defaultHandleRest
        | otherwise -> do -- SearchNodeBuilder
        deconss <- builderDeconss
        fromMaybe defaultHandleRest $ getFirst
                                    $ foldMap First
                                    $ deconss <&> \decons -> case decons of
          (matchParam, [(matchId, matchRs)], False) -> let
            resultTypes = map incF matchRs
            -- inputType = incF matchParam
            in unifyRightOffset vtResult (HsTypeOffset matchParam offset) <&> \substs -> do -- SearchNodeBuilder
              vars <- replicateM (length matchRs) builderAllocVar
              builderAddVarUsage v
              builderSetReason $ "pattern matching on " ++ showVar v
              let newProvTypes = map (applySubsts substs) resultTypes
                  newBinds = zipWith (\x y -> splitBinding (VarBinding x y)) vars newProvTypes
                  expr = ExpLetMatch matchId vars (ExpVar v) (ExpHole vid)
              builderFillExprHole vid expr
              when (not $ null matchRs)
                   (builderFixMaxTVarId $ maximum $ map largestId newProvTypes)
              addScopePatternMatch multiPM goalType vid sid $ reverse newBinds ++ bindingRest
          (matchParam, matchers@(_:_), False) | multiPM -> let
            -- inputType = incF matchParam
            in unifyRightOffset vtResult (HsTypeOffset matchParam offset) <&> \substs -> do -- SearchNodeBuilder
              mData <- matchers `forM` \(matchId, matchRs) -> do -- SearchNodeBuilder
                newSid <- builderAddScope sid
                let resultTypes = map incF matchRs
                vars <- replicateM (length matchRs) builderAllocVar
                builderAddVarUsage v
                newVid <- builderAllocHole
                let newProvTypes = map (applySubsts substs) resultTypes
                    newBinds = zipWith (\x y -> splitBinding (VarBinding x y)) vars newProvTypes
                when (not $ null matchRs)
                     (builderFixMaxTVarId $ maximum $ map largestId newProvTypes)
                return ((matchId, vars, ExpHole newVid), (newVid, reverse newBinds, newSid))
              builderSetReason $ "pattern matching on " ++ showVar v
              builderFillExprHole vid $ ExpCaseMatch (ExpVar v) (map fst mData)
              concat <$> map snd mData `forM` \(newVid, newBinds, newSid) ->
                addScopePatternMatch multiPM goalType newVid newSid (newBinds++bindingRest)
          _ -> Nothing -- TODO: decons for recursive data types
 where
  (<&>) = flip (<$>)
