-- {-# LANGUAGE TemplateHaskell #-}
module Infression where



import Type
import Unify
import TypeClasses
import ConstrainedType

import qualified Data.PQueue.Prio.Max as Q
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Arrow ( first, second )
import Data.Maybe ( maybeToList )

import Control.DeepSeq
import Data.DeriveTH
import Debug.Hood.Observe
import Debug.Trace

import Control.Arrow ( (***) )
import Control.Monad ( guard )
import Text.PrettyPrint

import Data.StableMemo



data Expression = ExpVar TVarId
                | ExpLit String
                | ExpLambda TVarId Expression
                | ExpApply Expression Expression
                | ExpHole TVarId

-- $( derive makeNFData ''Expression )

instance Show Expression where
  showsPrec _ (ExpVar i) = showString $ showVar i
  showsPrec _ (ExpLit s) = showString s
  showsPrec d (ExpLambda i e) =
    showParen (d>0) $ showString ("\\" ++ showVar i ++ " -> ") . showsPrec 1 e
  showsPrec d (ExpApply e1 e2) =
    showParen (d>1) $ showsPrec 2 e1 . showString " " . showsPrec 2 e2
  showsPrec _ (ExpHole i) = showString $ "_" ++ showVar i

fillExprHole :: TVarId -> Expression -> Expression -> Expression
fillExprHole vid t (ExpHole j) | vid==j = t
fillExprHole vid t (ExpLambda i ty) = ExpLambda i $ fillExprHole vid t ty
fillExprHole vid t (ExpApply e1 e2) = ExpApply (fillExprHole vid t e1)
                                              (fillExprHole vid t e2)
fillExprHole _ _ t = t

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
  observer state parent = observeOpaque (show state) state parent

type RatedStates = Q.MaxPQueue Float State

findExpression :: HsConstrainedType -> [(String, HsConstrainedType)] -> StaticContext -> [(Int, Float, Expression)]
findExpression (HsConstrainedType cs t) funcs staticContext =
  findExpression' 0 $ Q.singleton 100000.0 $ State
        [((0, t), [])]
        (map splitFunctionType funcs)
        (mkDynContext staticContext cs)
        (ExpHole 0)
        1
        (largestId t)
        0.0
        Nothing

findExpression' :: Int -> RatedStates -> [(Int,Float,Expression)]
findExpression' n states =
  if Q.null states
    then []
    else
      let ((_,s), restStates) = Q.deleteFindMax states
      in if null (goals s)
        then -- trace (showStateDevelopment s) $
          (n, depth s, expression s) : (findExpression' (n+1) restStates)
        else
          let resultStates = stateStep s
          in findExpression' (n+1)
               $ foldr (uncurry Q.insert) restStates
               $ [(r, newS) | newS <- resultStates, let r = rateState newS]

rateState :: State -> Float
rateState s = 0.0 - fromIntegral (length $ goals s) - depth s

stateStep :: State -> [State]
stateStep s = -- traceShow s $
  if depth s > 50.0 then [] else
  let
    (((var, goalType),binds):gr) = goals s -- [] should not be possible
  in case goalType of
      (TypeArrow t1 t2) ->
        let v1 = nextVarId s
            v2 = v1+1
            newNext = v2+1
        in (:[]) $ State
          (newGoal (v2, t2) [(v1, t1)] binds:gr)
          (functions s)
          (context s)
          (fillExprHole var (ExpLambda v1 (ExpHole v2)) $ expression s)
          newNext
          (maxTVarId s)
          (depth s + 0.5)
          (Just s)
      _ ->
        let
          byProvided = do
            (provId, provT, provPs) <- concat $ binds
            byGenericUnify
              (ExpVar provId)
              provT
              (S.toList $ dynContext_constraints $ context s)
              provPs
              0.1
          byFunction = do
            (funcId, funcR, funcParams, funcConstrs) <- functions s
            let incF = incVarIds (+(1+maxTVarId s))
            byGenericUnify
              (ExpLit funcId)
              (incF funcR)
              (map (constraintMapTypes incF) funcConstrs)
              (map incF funcParams)
              3.0
          byGenericUnify :: Expression -> HsType -> [Constraint] -> [HsType] -> Float -> [State]
          byGenericUnify coreExp provided provConstrs dependencies depthMod = do
            substs <- maybeToList $ unify goalType provided
            let contxt = context s
                constrs = map (constraintApplySubsts substs) $ S.toList $ dynContext_constraints $ context s
            guard $ isProvable contxt constrs                     
            let
              vBase = nextVarId s
              paramN = length dependencies
              expr = case paramN of
                0 -> coreExp
                n -> foldl ExpApply coreExp (map ExpHole [vBase..vBase+n-1])
              newGoals = zip 
                           (zip [vBase..] $ map (applySubsts substs) dependencies)
                           (repeat $ map (map $ varPBindingApplySubsts substs) binds)
              newConstraints = map (constraintApplySubsts substs) provConstrs
              newContext = mkDynContext
                (dynContext_context $ context s)
                (newConstraints ++ (S.toList $ dynContext_constraints $ context s))
            return $ State
              (newGoals ++ map (goalApplySubst substs) gr)
              (functions s)
              newContext
              (fillExprHole var expr $ expression s)
              (vBase + paramN)
              (max (maxTVarId s) (largestSubstsId substs))
              (depth s + depthMod)
              (Just s)
        in byProvided ++ byFunction

splitFunctionType :: (a, HsConstrainedType) -> (a, HsType, [HsType], [Constraint])
splitFunctionType (a,HsConstrainedType constrs b) = let (c,d) = f b in (a,c,d,constrs)
  where
    f :: HsType -> (HsType, [HsType])
    f (TypeArrow t1 t2) = let (c',d') = f t2 in (c', t1:d')
    f t = (t, [])

splitBinding :: (a, HsType) -> (a, HsType, [HsType])
splitBinding (a,b) = let (c,d) = f b in (a,c,d)
  where
    f :: HsType -> (HsType, [HsType])
    f (TypeArrow t1 t2) = let (c',d') = f t2 in (c', t1:d')
    f t = (t, [])
