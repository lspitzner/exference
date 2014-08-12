{-# LANGUAGE TemplateHaskell #-}
module Infression where



import Type
import Unify
-- import qualified Data.Map as M
import qualified Data.PQueue.Prio.Max as Q
import Control.Arrow ( second )
import Data.Maybe ( maybeToList )

import Control.DeepSeq
import Data.DeriveTH
import Debug.Hood.Observe
import Debug.Trace



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

data State = State
  { goals :: [VarBinding]
  , provided :: [VarBinding]
  , functions :: [FuncBinding]
  , expression :: Expression
  , nextVarId :: TVarId
  , maxTVarId :: TVarId
  , depth :: Float
  }
  deriving Show

instance Observable State where
  observer state parent = observeOpaque (show state) state parent

type RatedStates = Q.MaxPQueue Float State

findExpression :: HsType -> [(String, HsType)] -> [Expression]
findExpression t funcs =
  findExpression' $ Q.singleton 100000.0 $ State
        [(0, t)]
        []
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
        then (expression s):findExpression' restStates
        else
          let resultStates = stateStep s
          in findExpression' $ foldr (uncurry Q.insert) restStates
                             $ [(r, s) | s <- resultStates, let r = rateState s]

rateState :: State -> Float
rateState s = 0.0 - fromIntegral (length $ goals s) - depth s

stateStep :: State -> [State]
stateStep s = traceShow s $ if depth s > 13.0 then [] else
  let
    ((var, goalType):gr) = goals s -- [] should not be possible
  in case goalType of
      (TypeArrow t1 t2) ->
        let v1 = nextVarId s
            v2 = v1+1
            newNext = v2+1
        in (:[]) $ State
          ((v2, t2):gr)
          ((v1, t1):provided s)
          (functions s)
          (fillExprHole var (ExpLambda v1 (ExpHole v2)) $ expression s)
          newNext
          (maxTVarId s)
          (depth s + 0.1)
      t ->
        let
          byProvided = do
            (provId, provT) <- provided s
            substs <- maybeToList $ unify goalType provT
            let bindingApply = map (second $ applySubsts substs)
            return $ State
              (bindingApply gr)
              (bindingApply $ provided s)
              (functions s)
              (fillExprHole var (ExpVar provId) $ expression s)
              (nextVarId s)
              (max (maxTVarId s) (largestSubstsId substs))
              (depth s + 1.0)
          byFunction = do
            (funcId, funcR, funcParams) <- functions s
            let incF = incVarIds (+(1+maxTVarId s))
            substs <- maybeToList $ unify goalType $ incF funcR
            let
              vBase = nextVarId s
              lit = ExpLit funcId
              paramN = length funcParams
              expr = case paramN of
                0 -> lit
                n -> foldl ExpApply lit (map ExpHole [vBase..vBase+n-1])
              substsApply = applySubsts substs
              bindingApply = map (second substsApply)
              newGoals = zip [vBase..] $ map (substsApply.incF) funcParams
            trace ("substs="++show substs) $ return $ State
              (newGoals ++ bindingApply gr)
              (bindingApply $ provided s)
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