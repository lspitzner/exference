{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.Exference.Internal.ExferenceNode
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



import Language.Haskell.Exference.Type
import Language.Haskell.Exference.TypeClasses
import Language.Haskell.Exference.Expression
import Language.Haskell.Exference.ExferenceStats
import Language.Haskell.Exference.FunctionBinding

import qualified Data.Map.Strict as M

import Text.PrettyPrint
import Data.StableMemo

import Control.Arrow ( first, second, (***) )

import Control.DeepSeq.Generics
import GHC.Generics

import Debug.Hood.Observe



type VarBinding = (TVarId, HsType)
type VarPBinding = (TVarId, HsType, [HsType])
                -- var, result, params


varBindingApplySubsts :: Substs -> VarBinding -> VarBinding
varBindingApplySubsts = second . applySubsts

varPBindingApplySubsts :: Substs -> VarPBinding -> VarPBinding
varPBindingApplySubsts ss (a,b,c) =
  ( a
  , applySubsts ss b
  , map (applySubsts ss) c
  )

type ScopeId = Int

data Scope = Scope [VarPBinding] [ScopeId]
            -- scope bindings,   superscopes
  deriving Generic
data Scopes = Scopes ScopeId (M.Map ScopeId Scope)
  deriving Generic
              -- next id     scopes

initialScopes :: Scopes
initialScopes = Scopes 1 (M.singleton 0 $ Scope [] [])

scopeGetAllBindings :: Scopes -> Int -> [VarPBinding]
scopeGetAllBindings ss@(Scopes _ scopeMap) sid =
  case M.lookup sid scopeMap of
    Nothing -> []
    Just (Scope binds ids) -> binds ++ concatMap (scopeGetAllBindings ss) ids

scopesApplySubsts :: Substs -> Scopes -> Scopes
scopesApplySubsts substs (Scopes i scopeMap) = Scopes i $ M.map scopeF scopeMap
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
    newMap = M.adjust addBinding sid sMap
    addBinding :: Scope -> Scope
    addBinding (Scope vbinds ids) = Scope (binding:vbinds) ids

addScope :: ScopeId -> Scopes -> (ScopeId, Scopes)
addScope parent (Scopes nid sMap) = (nid, Scopes (nid+1) newMap)
  where
    newMap   = M.insert nid newScope sMap
    newScope = Scope [] [parent]

type VarUsageMap = M.Map TVarId Int

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
    newMap = M.insert nid (Scope transformedBinds [sid]) sMap
    transformedBinds = map splitBinding givenBinds

addNewScopeGoal :: ScopeId -> VarBinding -> Scopes -> (TGoal, ScopeId, Scopes)
addNewScopeGoal sid goalBind (Scopes nid sMap) =
    ((goalBind, nid), nid, Scopes (nid+1) newMap)
  where
    newMap = M.insert nid (Scope [] [sid]) sMap

mkGoals :: ScopeId
        -> [VarBinding]
        -> [TGoal]
mkGoals sid vbinds = [(b,sid)|b<-vbinds]

data SearchNode = SearchNode
  { node_goals           :: [TGoal]
  , node_constraintGoals :: [HsConstraint]
  , node_providedScopes  :: Scopes
  , node_varUses         :: VarUsageMap
  , node_functions       :: [FunctionBinding]
  , node_deconss         :: [DeconstructorBinding]
  , node_queryClassEnv   :: QueryClassEnv
  , node_expression      :: Expression
  , node_nextVarId       :: TVarId
  , node_maxTVarId       :: TVarId
  , node_depth           :: Float
#if LINK_NODES
  , node_previousNode    :: Maybe SearchNode
#endif
  , node_lastStepReason  :: String
  , node_lastStepBinding :: Maybe String
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
           <+> brackets (vcat $ punctuate (text ", ") $ map tgoal sgoals)
          )
      $$  (text $ "constrGoals= " ++ show scgoals)
      $$  (text   "scopes     = "
           <+> brackets (vcat $ punctuate (text ", ") $ map tScope $ M.toList scopeMap)
          )
      $$  (text $ "classEnv   = " ++ show qClassEnv)
      $$  (text $ "expression = " ++ show sexpression)
      $$  (text $ "reason     = " ++ reason)
      $$  (parens $    (text $ "nextVarId="++show snextVarId)
                   <+> (text $ "maxTVarId="++show smaxTVarId)
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
      tVarPType :: (TVarId, HsType, [HsType]) -> Doc
      tVarPType (i, t, ps) = tVarType (i, foldr TypeArrow t ps)

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

splitBinding :: (a, HsType) -> (a, HsType, [HsType])
splitBinding (a,b) = let (c,d) = f b in (a,c,d)
  where
    f :: HsType -> (HsType, [HsType])
    f (TypeArrow t1 t2) = let (c',d') = f t2 in (c', t1:d')
    f t = (t, [])
