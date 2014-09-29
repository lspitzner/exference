module Language.Haskell.Exference.Internal.ExferenceState
  ( State (..)
  , FuncDictElem (..)
  , TGoal
  , Scopes (..)
  , Scope (..)
  , ScopeId
  , VarPBinding
  , VarUsageMap
  , varBindingApplySubsts
  , varPBindingApplySubsts
  , goalApplySubst
  , showStateDevelopment
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

import qualified Data.Map.Strict as M

import Text.PrettyPrint
import Data.StableMemo

import Control.Arrow ( first, second, (***) )

import Debug.Hood.Observe



type VarBinding = (TVarId, HsType)
data FuncDictElem = SimpleBinding String Float HsType [HsType] [Constraint]
                              -- name, results, params constraints
                  | MatchBinding  String [HsType] HsType
  deriving Show

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
data Scopes = Scopes ScopeId (M.Map ScopeId Scope)
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

data State = State
  { state_goals           :: [TGoal]
  , state_constraintGoals :: [Constraint]
  , state_providedScopes  :: Scopes
  , state_varUses         :: VarUsageMap
  , state_functions       :: [FuncDictElem]
  , state_context         :: DynContext
  , state_expression      :: Expression
  , state_nextVarId       :: TVarId
  , state_maxTVarId       :: TVarId
  , state_depth           :: Float
  , state_previousState   :: Maybe State
  , state_lastStepReason  :: String
  , state_lastStepBinding :: Maybe String
  }

instance Show State where
  show (State sgoals
              scgoals
              (Scopes _ scopeMap)
              _svarUses
              _sfuncs
              scontext
              sexpression
              snextVarId
              smaxTVarId
              sdepth
              _prev
              reason
              _lastStepBinding)
    = show
    $ text "State" <+> (
          (text   "goals      ="
           <+> brackets (vcat $ punctuate (text ", ") $ map tgoal sgoals)
          )
      $$  (text $ "constrGoals= " ++ show scgoals)
      $$  (text   "scopes     = "
           <+> brackets (vcat $ punctuate (text ", ") $ map tScope $ M.toList scopeMap)
          )
      $$  (text $ "context    = " ++ show scontext)
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

showStateDevelopment :: State -> String
showStateDevelopment s = maybe "" f (state_previousState s) ++ show s
  where
    f :: State -> String
    f x = showStateDevelopment x ++ "\n"

instance Observable State where
  observer state = observeOpaque (show state) state

splitBinding :: (a, HsType) -> (a, HsType, [HsType])
splitBinding (a,b) = let (c,d) = f b in (a,c,d)
  where
    f :: HsType -> (HsType, [HsType])
    f (TypeArrow t1 t2) = let (c',d') = f t2 in (c', t1:d')
    f t = (t, [])
