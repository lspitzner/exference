{-# LANGUAGE TemplateHaskell #-}
module Infression where



import Type
import Unify
-- import qualified Data.Map as M
import qualified Data.PQueue.Prio.Max as Q
import qualified Data.Map as M
import Control.Arrow ( first, second )
import Data.Maybe ( maybeToList )

import Control.DeepSeq
import Data.DeriveTH
import Debug.Hood.Observe
import Debug.Trace

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

type TGoal = (VarBinding, [[VarBinding]])
           -- goal,   list of scopes containing appliciable bindings

newGoal :: VarBinding -> [VarBinding] -> [[VarBinding]] -> TGoal
newGoal g b bs = (g,b:bs)

goalApplySubst :: Substs -> TGoal -> TGoal
goalApplySubst = memo2 f
  where
    f :: Substs -> TGoal -> TGoal
    f ss ((v,t),binds) = ((v, applySubsts ss t), map (map $ second $ applySubsts ss) binds)


data State = State
  { goals :: [TGoal]
  , functions :: [FuncBinding]
  , expression :: Expression
  , nextVarId :: TVarId
  , maxTVarId :: TVarId
  , depth :: Float
  }

instance Show State where
  show (State goals functions expression nextVarId maxTVarId depth)
    = show
    $ text "State" <+> (
          (text   "goals      ="
           <+> brackets (vcat $ punctuate (text ", ") $ map tgoal goals)
          )
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
                                               (map tVarType $ concat binds)
                            )
      tVarType :: (TVarId, HsType) -> Doc
      tVarType (i, t) = text $ showVar i ++ " :: " ++ show t

instance Observable State where
  observer state parent = observeOpaque (show state) state parent

type RatedStates = Q.MaxPQueue Float State

findExpression :: HsType -> [(String, HsType)] -> [Expression]
findExpression t funcs =
  findExpression' $ Q.singleton 100000.0 $ State
        [((0, t), [])]
        (map splitFunctionType funcs)
        (ExpHole 0)
        1
        (largestId t)
        0.0

findExpression' :: RatedStates -> [Expression]
findExpression' states =
  if Q.null states
    then []
    else
      let ((_,s), restStates) = Q.deleteFindMax states
      in if null (goals s)
        then traceShow s $ (expression s):findExpression' restStates
        else
          let resultStates = stateStep s
          in findExpression' $ foldr (uncurry Q.insert) restStates
                             $ [(r, s) | s <- resultStates, let r = rateState s]

rateState :: State -> Float
rateState s = 0.0 - fromIntegral (length $ goals s) - depth s

stateStep :: State -> [State]
stateStep s = traceShow s $ if depth s > 13.0 then [] else
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
          (fillExprHole var (ExpLambda v1 (ExpHole v2)) $ expression s)
          newNext
          (maxTVarId s)
          (depth s + 0.1)
      t ->
        let
          byProvided = do
            (provId, provT) <- concat $ binds
            substs <- maybeToList $ unify goalType provT
            let bindingApply = map (second $ applySubsts substs)
            return $ State
              (map (goalApplySubst substs) gr)
              (functions s)
              (fillExprHole var (ExpVar provId) $ expression s)
              (nextVarId s)
              (max (maxTVarId s) (largestSubstsId substs))
              (depth s + 1.0)
          byFunction = do
            (funcId, funcR, funcParams) <- functions s
            let incF = incVarIds (+(1+maxTVarId s))
            substs <- maybeToList $ unify (traceShowId goalType) $ traceShowId (incF funcR)
            let
              vBase = nextVarId s
              lit = ExpLit funcId
              paramN = length funcParams
              expr = case paramN of
                0 -> lit
                n -> foldl ExpApply lit (map ExpHole [vBase..vBase+n-1])
              substsApply = applySubsts substs
              bindingApply = map (second substsApply)
              newGoals = map (\g -> (g,map bindingApply binds))
                       $ zip [vBase..]
                       $ map (substsApply.incF) funcParams
            trace ("substs="++show (map (first showVar) $ M.toList substs)) $ return $ State
              (newGoals ++ map (goalApplySubst substs) gr)
              (functions s)
              (fillExprHole var expr $ expression s)
              (vBase + paramN)
              (max (maxTVarId s) (largestSubstsId substs))
              (depth s + 3.0)
        in trace ("provided="++show (length byProvided)
                ++", functionApp="++show (length byFunction))
          $ byProvided ++ byFunction

splitFunctionType :: (String, HsType) -> (String, HsType, [HsType])
splitFunctionType (a,b) = let (c,d) = f b in (a,c,d)
  where
    f :: HsType -> (HsType, [HsType])
    f (TypeArrow t1 t2) = let (c',d') = f t2 in (c', t1:d')
    f t = (t, [])