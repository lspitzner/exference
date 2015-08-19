module Language.Haskell.Exference.Core.ExpressionSimplify
  ( simplifyExpression
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.Expression

import Data.Function ( on )



simplifyExpression :: Expression -> Expression
simplifyExpression = simplifyId . simplifyCompose . simplifyEta . simplifyLets



simplifyLets :: Expression -> Expression
simplifyLets e@(ExpVar _)     = e
simplifyLets e@(ExpName _)    = e
simplifyLets (ExpLambda i e)  = ExpLambda i $ simplifyLets e
simplifyLets (ExpApply e1 e2) = ExpApply (simplifyLets e1) (simplifyLets e2)
simplifyLets e@(ExpHole _)    = e
simplifyLets (ExpLetMatch name vids bindExp inExp) =
  ExpLetMatch name vids (simplifyLets bindExp) (simplifyLets inExp)
simplifyLets (ExpLet i bindExp inExp) = case countUses i inExp of
  0 -> simplifyLets inExp
  1 -> simplifyLets $ replaceVar i bindExp inExp
  _ -> ExpLet i (simplifyLets bindExp) (simplifyLets inExp)
simplifyLets (ExpCaseMatch bindExp alts) =
  ExpCaseMatch (simplifyLets bindExp) [ (c, vars, simplifyLets expr)
                                      | (c, vars, expr) <- alts
                                      ]

simplifyEta :: Expression -> Expression
simplifyEta e@(ExpVar _) = e
simplifyEta e@(ExpName _) = e
simplifyEta (ExpLambda i e) = simplifyEta' $ ExpLambda i $ simplifyEta e
simplifyEta (ExpApply e1 e2) = ExpApply (simplifyEta e1) (simplifyEta e2)
simplifyEta e@(ExpHole _) = e
simplifyEta (ExpLetMatch name vids bindExp inExp) =
  ExpLetMatch name vids (simplifyEta bindExp) (simplifyEta inExp)
simplifyEta (ExpLet i bindExp inExp) =
  ExpLet i (simplifyEta bindExp) (simplifyEta inExp)
simplifyEta (ExpCaseMatch bindExp alts) =
  ExpCaseMatch (simplifyEta bindExp) [ (c, vars, simplifyEta expr)
                                     | (c, vars, expr) <- alts
                                     ]

simplifyEta' :: Expression -> Expression
simplifyEta' (ExpLambda i (ExpApply e1 (ExpVar j)))
  | i==j && 0==countUses i e1 = e1
simplifyEta' e = e


simplifyId :: Expression -> Expression
simplifyId e@(ExpVar _) = e
simplifyId e@(ExpName _) = e
simplifyId e@(ExpLambda i (ExpVar j)) | i == j = ExpName specialQName_id
                                      | otherwise = e
simplifyId (ExpLambda i e) = ExpLambda i $ simplifyId e
simplifyId (ExpApply e1 e2) = ExpApply (simplifyId e1) (simplifyId e2)
simplifyId e@(ExpHole _) = e
simplifyId (ExpLetMatch name vids bindExp inExp) =
  ExpLetMatch name vids (simplifyId bindExp) (simplifyId inExp)
simplifyId (ExpLet i bindExp inExp) =
  ExpLet i (simplifyId bindExp) (simplifyId inExp)
simplifyId (ExpCaseMatch bindExp alts) =
  ExpCaseMatch (simplifyId bindExp) [ (c, vars, simplifyId expr)
                                    | (c, vars, expr) <- alts
                                    ]

simplifyCompose :: Expression -> Expression
simplifyCompose e@(ExpVar _) = e
simplifyCompose e@(ExpName _) = e
simplifyCompose (ExpLambda i e) = simplifyCompose' i [] e
simplifyCompose (ExpApply e1 e2) = ExpApply (simplifyCompose e1) (simplifyCompose e2)
simplifyCompose e@(ExpHole _) = e
simplifyCompose (ExpLetMatch name vids bindExp inExp) =
  ExpLetMatch name vids (simplifyCompose bindExp) (simplifyCompose inExp)
simplifyCompose (ExpLet i bindExp inExp) =
  ExpLet i (simplifyCompose bindExp) (simplifyCompose inExp)
simplifyCompose (ExpCaseMatch bindExp alts) =
  ExpCaseMatch (simplifyCompose bindExp) [ (c, vars, simplifyCompose expr)
                                      | (c, vars, expr) <- alts
                                      ]

simplifyCompose' :: Int -> [Expression] -> Expression -> Expression
simplifyCompose' i [] (ExpVar j) = ExpLambda i (ExpVar j)
simplifyCompose' i l e@(ExpVar j)
  | i == j    = foldl1 (\e1 e2 -> ExpApply
                                    (ExpApply
                                      (ExpName specialQName_compose) e2) e1)
                       l
  | otherwise = ExpLambda i $ foldl (flip ExpApply) e l
simplifyCompose' i l (ExpApply e1 e2)
  | countUses i e1==0 = simplifyCompose' i (simplifyCompose e1:l) e2
simplifyCompose' i l e = ExpLambda i $ foldl (flip ExpApply) (simplifyCompose e) l

replaceVar :: TVarId -> Expression -> Expression -> Expression
replaceVar vid t orig@(ExpVar j) | vid==j = t
                                 | otherwise = orig
replaceVar vid t (ExpLambda i ty) = ExpLambda i $ replaceVar vid t ty
replaceVar vid t (ExpApply e1 e2) = ExpApply (replaceVar vid t e1)
                                             (replaceVar vid t e2)
replaceVar vid t (ExpLetMatch n vars bindExp inExp) =
  ExpLetMatch n vars (replaceVar vid t bindExp) (replaceVar vid t inExp)
replaceVar vid t (ExpLet i bindExp inExp) =
  ExpLet i (replaceVar vid t bindExp) (replaceVar vid t inExp)
replaceVar vid t (ExpCaseMatch bindExp alts) =
  ExpCaseMatch (replaceVar vid t bindExp) [ (c, vars, replaceVar vid t expr)
                                          | (c, vars, expr) <- alts
                                          ]
replaceVar _ _ t@(ExpName _) = t
replaceVar _ _ t@(ExpHole _) = t


countUses :: TVarId -> Expression -> Int
countUses i (ExpVar j) | i==j = 1
                       | otherwise = 0
countUses _ (ExpName _) = 0
countUses i (ExpLambda j e) | i==j = 0
                            | otherwise = countUses i e
countUses i (ExpApply e1 e2) = ((+) `on` countUses i) e1 e2
countUses _ (ExpHole _) = 0
countUses i (ExpLetMatch _ _ bindExp inExp) = ((+) `on` countUses i) bindExp inExp
countUses i (ExpLet _ bindExp inExp) = ((+) `on` countUses i) bindExp inExp
countUses i (ExpCaseMatch bindExp alts) = sum $ countUses i bindExp
                                              : [ countUses i expr
                                                | (_, _, expr) <- alts
                                                ]

