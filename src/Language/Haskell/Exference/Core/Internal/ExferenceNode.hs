{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Language.Haskell.Exference.Core.Internal.ExferenceNode
  ( SearchNode (..)
  , TGoal
  , Scopes (..)
  , Scope (..)
  , ScopeId
  , VarPBinding
  , VarBinding (..)
  , VarUsageMap
  , varBindingApplySubsts
  , varPBindingApplySubsts
  , goalApplySubst
  , showNodeDevelopment
  , scopesApplySubsts
  , mkGoals
  , addScope
  , scopeGetAllBindings
  , scopesAddPBinding
  , splitBinding
  , addNewScopeGoal
  , initialScopes
  , addGoalProvided -- unused atm
  , showSearchNode
  -- SearchNode lenses
  , HasGoals (..)
  , HasConstraintGoals (..)
  , HasProvidedScopes (..)
  , HasVarUses (..)
  , HasFunctions (..)
  , HasDeconss (..)
  , HasQueryClassEnv (..)
  , HasExpression (..)
  , HasNextVarId (..)
  , HasMaxTVarId (..)
  , HasNextNVarId (..)
  , HasDepth (..)
#if LINK_NODES  
  , HasPreviousNode (..)
#endif
  , HasLastStepReason (..)
  , HasLastStepBinding (..)
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.ExferenceStats
import Language.Haskell.Exference.Core.FunctionBinding

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as V
import Data.Sequence
import Data.Foldable ( toList )
import Data.Functor.Identity ( runIdentity )

import Text.PrettyPrint

import Control.Arrow ( first, second, (***) )

import Control.DeepSeq.Generics
import Control.DeepSeq
import GHC.Generics
import Control.Lens.TH ( makeFields )

import Control.Monad.Trans.MultiRWS
import Control.Monad.Trans.MultiState ( runMultiStateTNil )
import Data.HList.ContainsType

import Debug.Hood.Observe

import Data.List ( intercalate )



data VarBinding = VarBinding {-# UNPACK #-} !TVarId HsType
 deriving (Generic)
type VarPBinding = (TVarId, HsType, [HsType], [TVarId], [HsConstraint])
                -- var, result, params, forallTypes, constraints


instance Show VarBinding where
  show (VarBinding vid ty) = showVar vid ++ " :-> " ++ show ty

varBindingApplySubsts :: Substs -> VarBinding -> VarBinding
varBindingApplySubsts substs (VarBinding v t) =
  VarBinding v (snd $ applySubsts substs t)

varPBindingApplySubsts :: Substs -> VarPBinding -> VarPBinding
varPBindingApplySubsts ss (v,rt,pt,fvs,cs) =
  let
    relevantSS = foldr IntMap.delete ss fvs
    (newResult, params, newForalls, newCs) = splitArrowResultParams
                                           $ snd
                                           $ applySubsts relevantSS rt
  in
  ( v
  , newResult
  , map (snd . applySubsts relevantSS) pt ++ params
  , newForalls ++ fvs
  , cs ++ newCs
  )

type ScopeId = Int

data Scope = Scope [VarPBinding] [ScopeId]
            -- scope bindings,   superscopes
  deriving Generic
data Scopes = Scopes ScopeId (IntMap.IntMap Scope)
  deriving Generic
              -- next id     scopes

instance Show Scope where
  show (Scope bindings sids) = "Scope " ++ show bindings ++ " " ++ show sids
instance Show Scopes where
  show (Scopes _sid intmap) = "Scopes\n" ++ intercalate ("\n") (fmap ("  " ++) $ fmap show $ IntMap.toAscList intmap)

initialScopes :: Scopes
initialScopes = Scopes 1 (IntMap.singleton 0 $ Scope [] [])

scopeGetAllBindings :: Int -> Scopes -> [VarPBinding]
scopeGetAllBindings sid ss@(Scopes _ scopeMap) =
  case IntMap.lookup sid scopeMap of
    Nothing -> []
    Just (Scope binds ids) -> binds ++ concatMap (`scopeGetAllBindings` ss) ids

scopesApplySubsts :: Substs -> Scopes -> Scopes
scopesApplySubsts substs (Scopes i scopeMap) = Scopes i $ IntMap.map scopeF scopeMap
  where
    scopeF (Scope binds ids) = Scope (map bindF binds) ids
    bindF = varPBindingApplySubsts substs

{-
scopesAddBinding :: ScopeId -> VarBinding -> Scopes -> Scopes
scopesAddBinding sid binding scopes =
  scopesAddPBinding sid (splitBinding binding) scopes
-}

scopesAddPBinding :: ScopeId -> VarPBinding -> Scopes -> Scopes
scopesAddPBinding sid binding (Scopes nid sMap) = Scopes nid newMap
  where
    newMap = IntMap.adjust addBinding sid sMap
    addBinding :: Scope -> Scope
    addBinding (Scope vbinds ids) = Scope (binding:vbinds) ids

addScope :: ScopeId -> Scopes -> (ScopeId, Scopes)
addScope parent (Scopes nid sMap) = (nid, Scopes (nid+1) newMap)
  where
    newMap   = IntMap.insert nid newScope sMap
    newScope = Scope [] [parent]

type VarUsageMap = IntMap.IntMap Int

type TGoal = (VarBinding, ScopeId)
           -- goal,    id of innermost scope available

goalApplySubst :: Substs -> TGoal -> TGoal
goalApplySubst ss | IntMap.null ss = id
                  | otherwise      = first (varBindingApplySubsts ss)

-- takes a new goal data, and a new set of provided stuff, and
-- returns some actual goal/newScope pair
addGoalProvided :: ScopeId
                -> VarBinding   -- goal binding
                -> [VarBinding] -- new-given-bindings
                -> Scopes
                -> (TGoal, Scopes)
addGoalProvided sid goalBind givenBinds (Scopes nid sMap) =
    ((goalBind, nid),Scopes (nid+1) newMap)
  where
    newMap = IntMap.insert nid (Scope transformedBinds [sid]) sMap
    transformedBinds = map splitBinding givenBinds

addNewScopeGoal :: ScopeId -> VarBinding -> Scopes -> (TGoal, ScopeId, Scopes)
addNewScopeGoal sid goalBind (Scopes nid sMap) =
    ((goalBind, nid), nid, Scopes (nid+1) newMap)
  where
    newMap = IntMap.insert nid (Scope [] [sid]) sMap

mkGoals :: ScopeId
        -> [VarBinding]
        -> [TGoal]
mkGoals sid vbinds = [(b,sid)|b<-vbinds]

data SearchNode = SearchNode
  { _searchNodeGoals           :: Seq TGoal
  , _searchNodeConstraintGoals :: [HsConstraint]
  , _searchNodeProvidedScopes  :: Scopes
  , _searchNodeVarUses         :: VarUsageMap
  , _searchNodeFunctions       :: V.Vector FunctionBinding
  , _searchNodeDeconss         :: [DeconstructorBinding]
  , _searchNodeQueryClassEnv   :: QueryClassEnv
  , _searchNodeExpression      :: Expression
  , _searchNodeNextVarId       :: {-# UNPACK #-} !TVarId
  , _searchNodeMaxTVarId       :: {-# UNPACK #-} !TVarId
  , _searchNodeNextNVarId      :: {-# UNPACK #-} !TVarId -- id used when resolving rankN-types
  , _searchNodeDepth           :: {-# UNPACK #-} !Float
#if LINK_NODES
  , _searchNodePreviousNode    :: Maybe SearchNode
#endif
  , _searchNodeLastStepReason  :: String
  , _searchNodeLastStepBinding :: Maybe String
  }
  deriving Generic

instance NFData VarBinding   where rnf = genericRnf
instance NFData Scope        where rnf = genericRnf
instance NFData Scopes       where rnf = genericRnf
instance NFData SearchNode   where rnf = genericRnf

-- instance Show SearchNode where
--   show (SearchNode sgoals
--               scgoals
--               (Scopes _ scopeMap)
--               _svarUses
--               _sfuncs
--               _sdeconss
--               qClassEnv
--               sexpression
--               snextVarId
--               smaxTVarId
--               snextNVarId
--               sdepth
-- #if LINK_NODES
--               _prev
-- #endif
--               reason
--               _lastStepBinding
--               )
--     = show
--     $ text "SearchNode" <+> (
--           (text   "goals      ="
--            <+> brackets (vcat $ punctuate (text ", ") $ map tgoal $ toList sgoals)
--           )
--       $$  (text $ "constrGoals= " ++ show scgoals)
--       $$  (text   "scopes     = "
--            <+> brackets (vcat $ punctuate (text ", ") $ map tScope $ IntMap.toList scopeMap)
--           )
--       $$  (text $ "classEnv   = " ++ show qClassEnv)
--       $$  (text $ "expression = " ++ showExpression sexpression)
--       $$  (text $ "reason     = " ++ reason)
--       $$  (parens $    (text $ "nextVarId="++show snextVarId)
--                    <+> (text $ "maxTVarId="++show smaxTVarId)
--                    <+> (text $ "nextNVarId="++show snextNVarId)
--                    <+> (text $ "depth="++show sdepth))
--     )
--     where
--       tgoal :: TGoal -> Doc
--       tgoal (vt,scopeId) =  tVarType vt
--                          <> text (" in " ++ show scopeId)
--       tScope :: (ScopeId, Scope) -> Doc
--       tScope (sid, Scope binds supers) =
--             text (show sid ++ " ")
--         <+> parens (text $ show $ supers)
--         <+> text " " <+> brackets (
--                               hcat $ punctuate (text ", ")
--                                                (map tVarPType binds)
--                             )
--       tVarType :: (TVarId, HsType) -> Doc
--       tVarType (i, t) = text $ showVar i ++ " :: " ++ show t
--       tVarPType :: (TVarId, HsType, [HsType], [TVarId], [HsConstraint]) -> Doc
--       tVarPType (i, t, ps, [], []) = tVarType (i, foldr TypeArrow t ps)
--       tVarPType (i, t, ps, fs, cs) = tVarType (i, TypeForall fs cs (foldr TypeArrow t ps))

showSearchNode :: SearchNode -> String
showSearchNode
  (SearchNode sgoals
              scgoals
              (Scopes _ scopeMap)
              _svarUses
              _sfuncs
              _sdeconss
              qClassEnv
              sexpression
              snextVarId
              smaxTVarId
              snextNVarId
              sdepth
#if LINK_NODES
              _prev
#endif
              reason
              _lastStepBinding
              ) =
  let exprStr = showExpression sexpression
  in show
    $ text "SearchNode" <+> (
          (text   "goals      ="
           <+> brackets (vcat $ punctuate (text ", ") $ map tgoal $ toList sgoals)
          )
      $$  (text $ "constrGoals= " ++ show scgoals)
      $$  (text   "scopes     = "
           <+> brackets (vcat $ punctuate (text ", ") $ map tScope $ IntMap.toList scopeMap)
          )
      $$  (text $ "classEnv   = " ++ show qClassEnv)
      $$  (text $ "expression = " ++ exprStr)
      $$  (text $ "reason     = " ++ reason)
      $$  (parens $    (text $ "nextVarId="++show snextVarId)
                   <+> (text $ "maxTVarId="++show smaxTVarId)
                   <+> (text $ "nextNVarId="++show snextNVarId)
                   <+> (text $ "depth="++show sdepth))
    )
 where
  tgoal :: TGoal -> Doc
  tgoal (vt,scopeId) =  tVarType vt
                     <> text (" in " ++ show scopeId)
  tScope :: (ScopeId, Scope) -> Doc
  tScope (sid, Scope binds supers) =
        text (show sid ++ " ")
    <+> parens (text $ show $ supers)
    <+> text " " <+> brackets (
                          hcat $ punctuate (text ", ")
                                           (map tVarPType binds)
                        )
  tVarType :: VarBinding -> Doc
  tVarType (VarBinding i t) = text $ showVar i ++ " :: " ++ show t
  tVarPType :: (TVarId, HsType, [HsType], [TVarId], [HsConstraint]) -> Doc
  tVarPType (i, t, ps, [], []) = tVarType $ VarBinding i (foldr TypeArrow t ps)
  tVarPType (i, t, ps, fs, cs) = tVarType $ VarBinding i (TypeForall fs cs (foldr TypeArrow t ps))

showNodeDevelopment :: SearchNode -> String
#if LINK_NODES
showNodeDevelopment s = case previousNode s of
  Nothing -> showSearchNode s
  Just p  -> do
    pStr <- showNodeDevelopment p
    cStr <- showSearchNode s
    return $ pStr ++ "\n" ++ cStr
#else
showNodeDevelopment _ = "[showNodeDevelopment: exference-core was not compiled with -fLinkNodes]"
#endif

-- instance Observable SearchNode where
--   observer state = observeOpaque (show state) state

splitBinding :: VarBinding -> VarPBinding
splitBinding (VarBinding v t) = let (rt,pts,fvs,cs) = splitArrowResultParams t in (v,rt,pts,fvs,cs)

makeFields ''SearchNode