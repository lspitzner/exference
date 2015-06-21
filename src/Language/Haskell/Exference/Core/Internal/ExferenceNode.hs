{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}

module Language.Haskell.Exference.Core.Internal.ExferenceNode
  ( SearchNode (..)
  , TGoal
  , Scopes (..)
  , Scope (..)
  , ScopeId
  , VarPBinding
  , VarBinding
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

import Text.PrettyPrint

import Control.Arrow ( first, second, (***) )

import Control.DeepSeq.Generics
import GHC.Generics

import Debug.Hood.Observe



type VarBinding = (TVarId, HsType)
type VarPBinding = (TVarId, HsType, [HsType], [TVarId], [HsConstraint])
                -- var, result, params, forallTypes, constraints


varBindingApplySubsts :: Substs -> VarBinding -> VarBinding
varBindingApplySubsts = second . applySubsts

varPBindingApplySubsts :: Substs -> VarPBinding -> VarPBinding
varPBindingApplySubsts ss (v,rt,pt,fvs,cs) =
  let
    relevantSS = foldr IntMap.delete ss fvs
    (newResult, params, newForalls, newCs) = splitArrowResultParams
                                           $ applySubsts relevantSS rt
  in
  ( v
  , newResult
  , map (applySubsts relevantSS) pt ++ params
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

initialScopes :: Scopes
initialScopes = Scopes 1 (IntMap.singleton 0 $ Scope [] [])

scopeGetAllBindings :: Scopes -> Int -> [VarPBinding]
scopeGetAllBindings ss@(Scopes _ scopeMap) sid =
  case IntMap.lookup sid scopeMap of
    Nothing -> []
    Just (Scope binds ids) -> binds ++ concatMap (scopeGetAllBindings ss) ids

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
goalApplySubst = first . varBindingApplySubsts

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
  { node_goals           :: Seq TGoal
  , node_constraintGoals :: [HsConstraint]
  , node_providedScopes  :: Scopes
  , node_varUses         :: VarUsageMap
  , node_functions       :: (V.Vector FunctionBinding)
  , node_deconss         :: [DeconstructorBinding]
  , node_queryClassEnv   :: QueryClassEnv
  , node_expression      :: Expression
  , node_nextVarId       :: {-# UNPACK #-} !TVarId
  , node_maxTVarId       :: {-# UNPACK #-} !TVarId
  , node_nextNVarId      :: {-# UNPACK #-} !TVarId -- id used when resolving rankN-types
  , node_depth           :: {-# UNPACK #-} !Float
#if LINK_NODES
  , node_previousNode    :: Maybe SearchNode
#endif
  , node_lastStepReason  :: !!String
  , node_lastStepBinding :: {-# UNPACK #-} !(Maybe String)
  }
  deriving Generic

instance NFData Scope        where rnf = genericRnf
instance NFData Scopes       where rnf = genericRnf
instance NFData SearchNode   where rnf = genericRnf

instance Show SearchNode where
  show (SearchNode sgoals
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
              )
    = show
    $ text "SearchNode" <+> (
          (text   "goals      ="
           <+> brackets (vcat $ punctuate (text ", ") $ map tgoal $ toList sgoals)
          )
      $$  (text $ "constrGoals= " ++ show scgoals)
      $$  (text   "scopes     = "
           <+> brackets (vcat $ punctuate (text ", ") $ map tScope $ IntMap.toList scopeMap)
          )
      $$  (text $ "classEnv   = " ++ show qClassEnv)
      $$  (text $ "expression = " ++ show sexpression)
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
      tVarType :: (TVarId, HsType) -> Doc
      tVarType (i, t) = text $ showVar i ++ " :: " ++ show t
      tVarPType :: (TVarId, HsType, [HsType], [TVarId], [HsConstraint]) -> Doc
      tVarPType (i, t, ps, [], []) = tVarType (i, foldr TypeArrow t ps)
      tVarPType (i, t, ps, fs, cs) = tVarType (i, TypeForall fs cs (foldr TypeArrow t ps))

showNodeDevelopment :: SearchNode -> String
#if LINK_NODES
showNodeDevelopment s = maybe "" f (node_previousNode s) ++ show s
  where
    f :: SearchNode -> String
    f x = showNodeDevelopment x ++ "\n"
#else
showNodeDevelopment _ = "[showNodeDevelopment: exference-core was not compiled with -fLinkNodes]"
#endif

instance Observable SearchNode where
  observer state = observeOpaque (show state) state

splitBinding :: (TVarId, HsType) -> VarPBinding
splitBinding (v,t) = let (rt,pts,fvs,cs) = splitArrowResultParams t in (v,rt,pts,fvs,cs)
