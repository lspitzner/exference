{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Exference.Core.Internal.ExferenceNodeBuilder
  ( SearchNodeBuilder
  , modifyNodeBy
  , builderSetReason
  , builderGetTVarOffset
  , builderAddScope
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
                                , modify
                                , get
                                , put
                                )
import Control.Applicative
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as V

import Control.Lens



type SearchNodeBuilder a = State SearchNode a

modifyNodeBy :: SearchNode -> SearchNodeBuilder () -> SearchNode
modifyNodeBy = flip execState

{-
builderAddVars :: [TVarId] -> SearchNodeBuilder ()
builderAddVars = (node_varUses <>=) . M.fromList . map (,0)
-}

-- sets reason, and, as appropriate, lastNode
builderSetReason :: String -> SearchNodeBuilder ()
builderSetReason r = do
  node_lastStepReason .= r
#if LINK_NODES
  node_previousNode <~ Just <$> get
#endif

builderGetTVarOffset :: SearchNodeBuilder TVarId
builderGetTVarOffset = (+1) <$> use node_maxTVarId
 -- TODO: is (+1) really necessary? it was in pre-transformation code,
 --       but i cannot find good reason now.. test?

-- take the current scope, add new scope, return new id
builderAddScope :: ScopeId -> SearchNodeBuilder ScopeId
builderAddScope parentId = do
  (newId, newScopes) <- uses node_providedScopes $ addScope parentId
  node_providedScopes .= newScopes
  return newId

-- apply substs in goals and scopes
-- not contraintGoals, because that's handled by caller
builderApplySubst :: Substs -> SearchNodeBuilder ()
builderApplySubst substs = do
  node_goals . mapped %= goalApplySubst substs
  node_providedScopes %= scopesApplySubsts substs

