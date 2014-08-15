{-# LANGUAGE TemplateHaskell #-}
module Infression where



import Type
import Unify
import TypeClasses
import ConstrainedType

import qualified Data.PQueue.Prio.Max as Q
import qualified Data.Map as M
import Control.Arrow ( first, second )
import Data.Maybe ( maybeToList )

import Control.DeepSeq
import Data.DeriveTH
import Debug.Hood.Observe
import Debug.Trace

import Control.Arrow ( (***) )

import Text.PrettyPrint

import Data.StableMemo



data Expression = ExpVar TVarId
                | ExpLit String
                | ExpLambda TVarId Expression
                | ExpApply Expression Expression
                | ExpHole TVarId

$( derive makeNFData ''Expression )

instance Show Expression where
  showsPrec _ (ExpVar i) = showString $ showVar i
  showsPrec _ (ExpLit s) = showString s
  showsPrec d (ExpLambda i e) =
    showParen (d>0) $ showString ("\\" ++ showVar i ++ " -> ") . showsPrec 1 e
  showsPrec d (ExpApply e1 e2) =
    showParen (d>1) $ showsPrec 2 e1 . showString " " . showsPrec 2 e2
  showsPrec d (ExpHole i) = showString $ "_" ++ showVar i

fillExprHole :: TVarId -> Expression -> Expression -> Expression
fillExprHole id t (ExpHole j) | id==j = t
fillExprHole id t (ExpLambda i ty) = ExpLambda i $ fillExprHole id t ty
fillExprHole id t (ExpApply e1 e2) = ExpApply (fillExprHole id t e1)
                                              (fillExprHole id t e2)
fillExprHole _ _ t = t

type VarBinding = (TVarId, HsType)
type FuncBinding = (String, HsType, [HsType]) -- name, result, params
type VarPBinding = (TVarId, HsType, [HsType]) -- var,  result, params

varBindingApplySubsts :: Substs -> VarBinding -> VarBinding
varBindingApplySubsts = second . applySubsts

varPBindingApplySubsts :: Substs -> VarPBinding -> VarPBinding
varPBindingApplySubsts ss (a,b,c) = (a, applySubsts ss b, map (applySubsts ss) c)

type TGoal = (VarBinding, [[VarPBinding]])
           -- goal,   list of scopes containing appliciable bindings

newGoal :: VarBinding -> [VarBinding] -> [[VarPBinding]] -> TGoal
newGoal g b bs = (g,map splitFunctionType b:bs)

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
  }

instance Show State where
  show (State goals functions context expression nextVarId maxTVarId depth)
    = show
    $ text "State" <+> (
          (text   "goals      ="
           <+> brackets (vcat $ punctuate (text ", ") $ map tgoal goals)
          )
      $$  (text $ "context    = " ++ show context)
      $$  (text $ "expression = " ++ show expression)
      $$  (parens $    (text $ "nextVarId="++show nextVarId)
                   <+> (text $ "maxTVarId="++show maxTVarId)
                   <+> (text $ "depth="++show depth))
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

instance Observable State where
  observer state parent = observeOpaque (show state) state parent

type RatedStates = Q.MaxPQueue Float State

findExpression :: HsConstrainedType -> [(String, HsType)] -> StaticContext -> [(Int, Expression)]
findExpression (HsConstrainedType cs t) funcs staticContext =
  findExpression' 0 $ Q.singleton 100000.0 $ State
        [((0, t), [])]
        (map splitFunctionType funcs)
        (mkDynContext staticContext cs)
        (ExpHole 0)
        1
        (largestId t)
        0.0

findExpression' :: Int -> RatedStates -> [(Int,Expression)]
findExpression' n states =
  if Q.null states
    then []
    else
      let ((_,s), restStates) = Q.deleteFindMax states
      in if null (goals s)
        then traceShow s
          $ (n, expression s) : (findExpression' (n+1) restStates)
        else
          let resultStates = stateStep s
          in findExpression' (n+1)
               $ foldr (uncurry Q.insert) restStates
               $ [(r, s) | s <- resultStates, let r = rateState s]

rateState :: State -> Float
rateState s = 0.0 - fromIntegral (length $ goals s) - depth s

stateStep :: State -> [State]
stateStep s = traceShow s $ if depth s > 20.0 then [] else
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
          (depth s + 0.1)
      t ->
        let
          byProvided = do
            (provId, provT, provPs) <- concat $ binds
            byGenericUnify (ExpVar provId) provT provPs 1.0
          byFunction = do
            (funcId, funcR, funcParams) <- functions s
            let incF = incVarIds (+(1+maxTVarId s))
            byGenericUnify (ExpLit funcId) (incF funcR) (map incF funcParams) 3.0
          byGenericUnify :: Expression -> HsType -> [HsType] -> Float -> [State]
          byGenericUnify coreExp provided dependencies depthMod = do
            substs <- maybeToList $ unify goalType provided
            let
              vBase = nextVarId s
              paramN = length dependencies
              expr = case paramN of
                0 -> coreExp
                n -> foldl ExpApply coreExp (map ExpHole [vBase..vBase+n-1])
              newGoals = zip 
                           (zip [vBase..] $ map (applySubsts substs) dependencies)
                           (repeat $ map (map $ varPBindingApplySubsts substs) binds)
            return $ State
              (newGoals ++ map (goalApplySubst substs) gr)
              (functions s)
              (context s)
              (fillExprHole var expr $ expression s)
              (vBase + paramN)
              (max (maxTVarId s) (largestSubstsId substs))
              (depth s + depthMod)
        in trace ("provided="++show (length byProvided)
                ++", functionApp="++show (length byFunction))
          $ byProvided ++ byFunction

splitFunctionType :: (a, HsType) -> (a, HsType, [HsType])
splitFunctionType (a,b) = let (c,d) = f b in (a,c,d)
  where
    f :: HsType -> (HsType, [HsType])
    f (TypeArrow t1 t2) = let (c',d') = f t2 in (c', t1:d')
    f t = (t, [])