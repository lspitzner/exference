{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Haskell.Exference.Internal.ExferenceNodeBuilder
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
  , builderSetConstraints
  , builderApplySubst
  )
where



import Language.Haskell.Exference.Types
import Language.Haskell.Exference.Expression
import Language.Haskell.Exference.Internal.ExferenceNode
import Language.Haskell.Exference.FunctionBinding

import Control.Monad.State.Lazy ( State
                                , execState
                                , modify
                                , get
                                , put
                                )
import Control.Applicative
import qualified Data.Map as M



newtype SearchNodeBuilder a = SearchNodeBuilder (State SearchNode a)
  deriving (Functor, Applicative, Monad)

modifyNodeBy :: SearchNode -> SearchNodeBuilder a -> SearchNode
modifyNodeBy n (SearchNodeBuilder b) = execState b n

builderPrependGoal :: TGoal -> SearchNodeBuilder ()
builderPrependGoal g = SearchNodeBuilder $ modify $ \s ->
  s { node_goals = g : node_goals s }

builderAppendGoal :: TGoal -> SearchNodeBuilder ()
builderAppendGoal g = SearchNodeBuilder $ modify $ \s ->
  s { node_goals = node_goals s ++ [g] }

builderAddConstraintGoals :: [HsConstraint] -> SearchNodeBuilder ()
builderAddConstraintGoals gs = SearchNodeBuilder $ modify $ \s ->
  s { node_constraintGoals = node_constraintGoals s ++ gs }

{-
builderAddVars :: [TVarId] -> SearchNodeBuilder ()
builderAddVars vids = SearchNodeBuilder
                    $ modify
                    $ \s -> s { node_varUses
                              = M.fromList [(vid,0)|vid<-vids]
                                `M.union` node_varUses s
                              }
-}

builderAddVarUsage :: TVarId -> SearchNodeBuilder ()
builderAddVarUsage v = SearchNodeBuilder $ modify $ \s ->
  s { node_varUses = M.adjust (+1) v $ node_varUses s }
  -- yes, this looks like perfect usecase for lenses;
  -- still not worth the dependency.

builderFillExprHole :: TVarId -> Expression -> SearchNodeBuilder ()
builderFillExprHole vid e = SearchNodeBuilder $ modify $ \s ->
  s { node_expression = fillExprHole vid e $ node_expression s }

builderAddDepth :: Float -> SearchNodeBuilder ()
builderAddDepth f = SearchNodeBuilder $ modify $ \s ->
  s { node_depth = f + node_depth s }

-- sets reason, and, as appropriate, lastNode
builderSetReason :: String -> SearchNodeBuilder ()
builderSetReason r = SearchNodeBuilder $ modify $ \s ->
  s { node_lastStepReason = r
#if LINK_NODES
    , node_previousNode = Just s
#endif
    }

builderAddPBinding :: ScopeId -> VarPBinding -> SearchNodeBuilder ()
builderAddPBinding sid pbind = SearchNodeBuilder $ modify $ \s ->
  s { node_providedScopes = scopesAddPBinding sid pbind
    $ node_providedScopes s
    }

builderFunctions :: SearchNodeBuilder [FunctionBinding]
builderFunctions = SearchNodeBuilder $ node_functions <$> get

builderDeconss :: SearchNodeBuilder [DeconstructorBinding]
builderDeconss = SearchNodeBuilder $ node_deconss <$> get

builderGetTVarOffset :: SearchNodeBuilder TVarId
builderGetTVarOffset = SearchNodeBuilder $ (+1) . node_maxTVarId <$> get
 -- TODO: is (+1) really necessary? it was in pre-transformation code,
 --       but i cannot find good reason now.. test?

builderAllocHole :: SearchNodeBuilder TVarId
builderAllocHole = SearchNodeBuilder $ do
  s <- get
  let vid = node_nextVarId s
  put $ s { node_nextVarId = vid+1 }
  return vid

builderAllocVar :: SearchNodeBuilder TVarId
builderAllocVar = SearchNodeBuilder $ do
  s <- get
  let vid = node_nextVarId s
  put $ s { node_nextVarId = vid+1
          , node_varUses = M.insert vid 0 $ node_varUses s }
  return vid

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

builderFixMaxTVarId :: Int -> SearchNodeBuilder ()
builderFixMaxTVarId bmax = SearchNodeBuilder $ modify $ \s ->
  s { node_maxTVarId = max bmax $ node_maxTVarId s }

builderSetLastStepBinding :: Maybe String -> SearchNodeBuilder ()
builderSetLastStepBinding ms = SearchNodeBuilder $ modify $ \s ->
  s { node_lastStepBinding = ms }

-- take the current scope, add new scope, return new id
builderAddScope :: ScopeId -> SearchNodeBuilder ScopeId
builderAddScope parentId = SearchNodeBuilder $ do
  s <- get
  let (newId,newScopes) = addScope parentId (node_providedScopes s)
  put $ s { node_providedScopes = newScopes }
  return newId

-- apply substs in goals and scopes
-- not contraintGoals, because that's handled by caller
builderApplySubst :: Substs -> SearchNodeBuilder ()
builderApplySubst substs = SearchNodeBuilder $ modify $ \s ->
  s { node_goals = map (goalApplySubst substs) $ node_goals s
    , node_providedScopes = scopesApplySubsts substs $ node_providedScopes s
    }

builderSetConstraints :: [HsConstraint] -> SearchNodeBuilder ()
builderSetConstraints cgoals = SearchNodeBuilder $ modify $ \s ->
  s { node_constraintGoals = cgoals }
