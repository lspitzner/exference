module InfressionState where



import Type
import TypeClasses
import Expression

import Text.PrettyPrint
import Data.StableMemo

import Control.Arrow ( first, second, (***) )

import Debug.Hood.Observe



type VarBinding = (TVarId, HsType)
type FuncBinding = (String, HsType, [HsType], [Constraint]) -- name, result, params
type VarPBinding = (TVarId, HsType, [HsType]) -- var, result, constraints, params

varBindingApplySubsts :: Substs -> VarBinding -> VarBinding
varBindingApplySubsts = second . applySubsts

varPBindingApplySubsts :: Substs -> VarPBinding -> VarPBinding
varPBindingApplySubsts ss (a,b,c) =
  ( a
  , applySubsts ss b
  , map (applySubsts ss) c
  )

type TGoal = (VarBinding, [[VarPBinding]])
           -- goal,   list of scopes containing appliciable bindings

newGoal :: VarBinding -> [VarBinding] -> [[VarPBinding]] -> TGoal
newGoal g b bs = (g,map splitBinding b:bs)

goalApplySubst :: Substs -> TGoal -> TGoal
goalApplySubst = memo2 f
  where
    f :: Substs -> TGoal -> TGoal
    f ss ((v,t),binds) = ( (v, applySubsts ss t)
                         , map (map $ varPBindingApplySubsts ss) binds
                         )


data State = State
  { goals      :: [TGoal]
  , functions  :: [FuncBinding]
  , context    :: DynContext
  , expression :: Expression
  , nextVarId  :: TVarId
  , maxTVarId  :: TVarId
  , depth      :: Float
  , previousState :: Maybe State
  }

instance Show State where
  show (State sgoals _sfuncs scontext sexpression snextVarId smaxTVarId sdepth _prev)
    = show
    $ text "State" <+> (
          (text   "goals      ="
           <+> brackets (vcat $ punctuate (text ", ") $ map tgoal sgoals)
          )
      $$  (text $ "context    = " ++ show scontext)
      $$  (text $ "expression = " ++ show sexpression)
      $$  (parens $    (text $ "nextVarId="++show snextVarId)
                   <+> (text $ "maxTVarId="++show smaxTVarId)
                   <+> (text $ "depth="++show sdepth))
    )
    where
      tgoal :: TGoal -> Doc
      tgoal (vt,binds) =    tVarType vt
                         <> text " given "
                         <> brackets (
                              hcat $ punctuate (text ", ")
                                               (map tVarPType $ concat binds)
                            )
      tVarType :: (TVarId, HsType) -> Doc
      tVarType (i, t) = text $ showVar i ++ " :: " ++ show t
      tVarPType :: (TVarId, HsType, [HsType]) -> Doc
      tVarPType (i, t, ps) = tVarType (i, foldr TypeArrow t ps)

showStateDevelopment :: State -> String
showStateDevelopment s = maybe "" f (previousState s) ++ show s
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
