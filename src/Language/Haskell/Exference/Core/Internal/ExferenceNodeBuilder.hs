{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Haskell.Exference.Core.Internal.ExferenceNodeBuilder
  ( SearchNodeBuilder
  , modifyNodeBy
  , builderPrependGoal
  , builderAppendGoal
  , builderAddVarUsage
  , builderFillExprHole
  , builderAddDepth
  , builderSetReason
  , builderAddPBinding
  , builderFunctions
  , builderDeconss
  , builderGetTVarOffset
  , builderAllocVar
  , builderAllocHole
  , builderAllocNVar
  , builderFixMaxTVarId
  , builderSetLastStepBinding
  , builderAddScope
  , builderAddConstraintGoals
  , builderAddGivenConstraints
  , builderSetConstraints
  , builderApplySubst
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.Internal.ExferenceNode
import Language.Haskell.Exference.Core.FunctionBinding

import Control.Monad.State.CPS ( State
                                , StateT( StateT )
                                , execState
                               -- , modify
                               -- , get
                               -- , put
                                )
import Control.Applicative
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as V
import Data.Sequence

{-# INLINE modify #-}
modify :: (s -> s) -> StateT s m ()
modify f = StateT $ \s c -> c () (f s)

{-# INLINE get #-}
get :: StateT s m s
get = StateT $ \s c -> c s s

{-# INLINE put #-}
put :: s -> StateT s m ()
put x = StateT $ \_ c -> c () x


newtype SearchNodeBuilder a = SearchNodeBuilder (State SearchNode a)
  deriving (Functor, Applicative, Monad)

{-# INLINE modifyNodeBy #-}
modifyNodeBy :: SearchNode -> SearchNodeBuilder a -> SearchNode
modifyNodeBy n (SearchNodeBuilder b) = execState b n

{-# INLINE builderPrependGoal #-}
builderPrependGoal :: TGoal -> SearchNodeBuilder ()
builderPrependGoal g = SearchNodeBuilder $ modify $ \s ->
  s { node_goals = g <| node_goals s }

{-# INLINE builderAppendGoal #-}
builderAppendGoal :: TGoal -> SearchNodeBuilder ()
builderAppendGoal g = SearchNodeBuilder $ modify $ \s ->
  s { node_goals = node_goals s |> g }

{-# INLINE builderAddConstraintGoals #-}
builderAddConstraintGoals :: [HsConstraint] -> SearchNodeBuilder ()
builderAddConstraintGoals gs = SearchNodeBuilder $ modify $ \s ->
  s { node_constraintGoals = node_constraintGoals s ++ gs }

{-# INLINE builderAddGivenConstraints #-}
builderAddGivenConstraints :: [HsConstraint] -> SearchNodeBuilder ()
builderAddGivenConstraints cs = SearchNodeBuilder $ modify $ \s ->
  s { node_queryClassEnv = addQueryClassEnv cs $ node_queryClassEnv s }
{-
builderAddVars :: [TVarId] -> SearchNodeBuilder ()
builderAddVars vids = SearchNodeBuilder
                    $ modify
                    $ \s -> s { node_varUses
                              = M.fromList [(vid,0)|vid<-vids]
                                `M.union` node_varUses s
                              }
-}

{-# INLINE builderAddVarUsage #-}
builderAddVarUsage :: TVarId -> SearchNodeBuilder ()
builderAddVarUsage v = SearchNodeBuilder $ modify $ \s ->
  s { node_varUses = IntMap.adjust (+1) v $ node_varUses s }
  -- yes, this looks like perfect usecase for lenses;
  -- still not worth the dependency.

{-# INLINE builderFillExprHole #-}
builderFillExprHole :: TVarId -> Expression -> SearchNodeBuilder ()
builderFillExprHole vid e = SearchNodeBuilder $ modify $ \s ->
  s { node_expression = fillExprHole vid e $ node_expression s }

{-# INLINE builderAddDepth #-}
builderAddDepth :: Float -> SearchNodeBuilder ()
builderAddDepth f = SearchNodeBuilder $ modify $ \s ->
  s { node_depth = f + node_depth s }

-- sets reason, and, as appropriate, lastNode
{-# INLINE builderSetReason #-}
builderSetReason :: String -> SearchNodeBuilder ()
builderSetReason r = SearchNodeBuilder $ modify $ \s ->
  s { node_lastStepReason = r
#if LINK_NODES
    , node_previousNode = Just s
#endif
    }

{-# INLINE builderAddPBinding #-}
builderAddPBinding :: ScopeId -> VarPBinding -> SearchNodeBuilder ()
builderAddPBinding sid pbind = SearchNodeBuilder $ modify $ \s ->
  s { node_providedScopes = scopesAddPBinding sid pbind
    $ node_providedScopes s
    }

{-# INLINE builderFunctions #-}
builderFunctions :: SearchNodeBuilder (V.Vector FunctionBinding)
builderFunctions = SearchNodeBuilder $ node_functions <$> get

{-# INLINE builderDeconss #-}
builderDeconss :: SearchNodeBuilder [DeconstructorBinding]
builderDeconss = SearchNodeBuilder $ node_deconss <$> get

{-# INLINE builderGetTVarOffset #-}
builderGetTVarOffset :: SearchNodeBuilder TVarId
builderGetTVarOffset = SearchNodeBuilder $ (+1) . node_maxTVarId <$> get
 -- TODO: is (+1) really necessary? it was in pre-transformation code,
 --       but i cannot find good reason now.. test?

{-# INLINE builderAllocHole #-}
builderAllocHole :: SearchNodeBuilder TVarId
builderAllocHole = SearchNodeBuilder $ do
  s <- get
  let vid = node_nextVarId s
  put $ s { node_nextVarId = vid+1 }
  return vid

{-# INLINE builderAllocVar #-}
builderAllocVar :: SearchNodeBuilder TVarId
builderAllocVar = SearchNodeBuilder $ do
  s <- get
  let vid = node_nextVarId s
  put $ s { node_nextVarId = vid+1
          , node_varUses = IntMap.insert vid 0 $ node_varUses s }
  return vid

{-# INLINE builderAllocNVar #-}
builderAllocNVar :: SearchNodeBuilder TVarId
builderAllocNVar = SearchNodeBuilder $ do
  s <- get
  let vid = node_nextNVarId s
  put $ s { node_nextNVarId = vid+1 }
  return vid

{-
builderAllocVars :: Int -> SearchNodeBuilder [TVarId]
builderAllocVars n = SearchNodeBuilder $ do
  s <- get
  let vid = node_nextVarId s
  put $ s { node_nextVarId = vid+n }
  return $ take n $ [vid..]
-}

{-# INLINE builderFixMaxTVarId #-}
builderFixMaxTVarId :: Int -> SearchNodeBuilder ()
builderFixMaxTVarId bmax = SearchNodeBuilder $ modify $ \s ->
  s { node_maxTVarId = max bmax $ node_maxTVarId s }

{-# INLINE builderSetLastStepBinding #-}
builderSetLastStepBinding :: Maybe String -> SearchNodeBuilder ()
builderSetLastStepBinding ms = SearchNodeBuilder $ modify $ \s ->
  s { node_lastStepBinding = ms }

-- take the current scope, add new scope, return new id
{-# INLINE builderAddScope #-}
builderAddScope :: ScopeId -> SearchNodeBuilder ScopeId
builderAddScope parentId = SearchNodeBuilder $ do
  s <- get
  let (newId,newScopes) = addScope parentId (node_providedScopes s)
  put $ s { node_providedScopes = newScopes }
  return newId

-- apply substs in goals and scopes
-- not contraintGoals, because that's handled by caller
{-# INLINE builderApplySubst #-}
builderApplySubst :: Substs -> SearchNodeBuilder ()
builderApplySubst substs = SearchNodeBuilder $ modify $ \s ->
  s { node_goals = fmap (goalApplySubst substs) $ node_goals s
    , node_providedScopes = scopesApplySubsts substs $ node_providedScopes s
    }

{-# INLINE builderSetConstraints #-}
builderSetConstraints :: [HsConstraint] -> SearchNodeBuilder ()
builderSetConstraints cgoals = SearchNodeBuilder $ modify $ \s ->
  s { node_constraintGoals = cgoals }
