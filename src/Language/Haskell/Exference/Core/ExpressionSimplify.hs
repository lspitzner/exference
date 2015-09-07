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
simplifyLets e@ExpVar{}       = e
simplifyLets e@ExpName{}      = e
simplifyLets (ExpLambda i ty e)  = ExpLambda i ty $ simplifyLets e
simplifyLets (ExpApply e1 e2) = ExpApply (simplifyLets e1) (simplifyLets e2)
simplifyLets e@ExpHole{}      = e
simplifyLets (ExpLetMatch name vids bindExp inExp) =
  ExpLetMatch name vids (simplifyLets bindExp) (simplifyLets inExp)
simplifyLets (ExpLet i ty bindExp inExp) = case countUses i inExp of
  0 -> simplifyLets inExp
  1 -> simplifyLets $ replaceVar i bindExp inExp
  _ -> ExpLet i ty (simplifyLets bindExp) (simplifyLets inExp)
simplifyLets (ExpCaseMatch bindExp alts) =
  ExpCaseMatch (simplifyLets bindExp) [ (c, vars, simplifyLets expr)
                                      | (c, vars, expr) <- alts
                                      ]

simplifyEta :: Expression -> Expression
simplifyEta e@ExpVar{}         = e
simplifyEta e@ExpName{}        = e
simplifyEta (ExpLambda i ty e) = simplifyEta' $ ExpLambda i ty $ simplifyEta e
simplifyEta (ExpApply e1 e2)   = ExpApply (simplifyEta e1) (simplifyEta e2)
simplifyEta e@ExpHole{}        = e
simplifyEta (ExpLetMatch name vids bindExp inExp) =
  ExpLetMatch name vids (simplifyEta bindExp) (simplifyEta inExp)
simplifyEta (ExpLet i ty bindExp inExp) =
  ExpLet i ty (simplifyEta bindExp) (simplifyEta inExp)
simplifyEta (ExpCaseMatch bindExp alts) =
  ExpCaseMatch (simplifyEta bindExp) [ (c, vars, simplifyEta expr)
                                     | (c, vars, expr) <- alts
                                     ]

simplifyEta' :: Expression -> Expression
simplifyEta' (ExpLambda i _ (ExpApply e1 (ExpVar j _)))
  | i==j && 0==countUses i e1 = e1
simplifyEta' e = e


simplifyId :: Expression -> Expression
simplifyId e@ExpVar{}                 = e
simplifyId e@ExpName{}                = e
simplifyId e@(ExpLambda i _ (ExpVar j _))
  | i == j                            = ExpName specialQName_id
  | otherwise                         = e
simplifyId (ExpLambda i ty e)         = ExpLambda i ty $ simplifyId e
simplifyId (ExpApply e1 e2)           = ExpApply (simplifyId e1) (simplifyId e2)
simplifyId e@ExpHole{}                = e
simplifyId (ExpLetMatch name vids bindExp inExp)
                                      = ExpLetMatch name
                                                    vids
                                                    (simplifyId bindExp)
                                                    (simplifyId inExp)
simplifyId (ExpLet i ty bindExp inExp)   =
  ExpLet i ty (simplifyId bindExp) (simplifyId inExp)
simplifyId (ExpCaseMatch bindExp alts) =
  ExpCaseMatch (simplifyId bindExp) [ (c, vars, simplifyId expr)
                                    | (c, vars, expr) <- alts
                                    ]

simplifyCompose :: Expression -> Expression
simplifyCompose e@ExpVar{}         = e
simplifyCompose e@ExpName{}        = e
simplifyCompose (ExpLambda i ty e) = simplifyCompose' i ty [] e
simplifyCompose (ExpApply e1 e2)   = ExpApply (simplifyCompose e1) (simplifyCompose e2)
simplifyCompose e@ExpHole{}        = e
simplifyCompose (ExpLetMatch name vids bindExp inExp)
                                 = ExpLetMatch name
                                               vids
                                               (simplifyCompose bindExp)
                                               (simplifyCompose inExp)
simplifyCompose (ExpLet i ty bindExp inExp)
                                 = ExpLet i
                                          ty
                                          (simplifyCompose bindExp)
                                          (simplifyCompose inExp)
simplifyCompose (ExpCaseMatch bindExp alts)
                                 = ExpCaseMatch
                                     (simplifyCompose bindExp)
                                     [ (c, vars, simplifyCompose expr)
                                     | (c, vars, expr) <- alts
                                     ]

simplifyCompose' :: Int -> HsType -> [Expression] -> Expression -> Expression
simplifyCompose' i ty [] e@ExpVar{} = ExpLambda i ty e
simplifyCompose' i ty l e@(ExpVar j _)
  | i == j    = foldl1 (\e1 e2 -> ExpApply
                                    (ExpApply
                                      (ExpName specialQName_compose) e2) e1)
                       l
  | otherwise = ExpLambda i ty $ foldl (flip ExpApply) e l
simplifyCompose' i ty l (ExpApply e1 e2)
  | countUses i e1==0 = simplifyCompose' i ty (simplifyCompose e1:l) e2
simplifyCompose' i ty l e = ExpLambda i ty
                          $ foldl (flip ExpApply) (simplifyCompose e) l

replaceVar :: TVarId -> Expression -> Expression -> Expression
replaceVar vid t orig@(ExpVar j _) | vid==j = t
                                   | otherwise = orig
replaceVar vid t (ExpLambda i ty e) = ExpLambda i ty $ replaceVar vid t e
replaceVar vid t (ExpApply e1 e2) = ExpApply (replaceVar vid t e1)
                                             (replaceVar vid t e2)
replaceVar vid t (ExpLetMatch n vars bindExp inExp) =
  ExpLetMatch n vars (replaceVar vid t bindExp) (replaceVar vid t inExp)
replaceVar vid t (ExpLet i ty bindExp inExp) =
  ExpLet i ty (replaceVar vid t bindExp) (replaceVar vid t inExp)
replaceVar vid t (ExpCaseMatch bindExp alts) =
  ExpCaseMatch (replaceVar vid t bindExp) [ (c, vars, replaceVar vid t expr)
                                          | (c, vars, expr) <- alts
                                          ]
replaceVar _ _ t@(ExpName _) = t
replaceVar _ _ t@(ExpHole _) = t


countUses :: TVarId -> Expression -> Int
countUses i (ExpVar j _) | i==j           = 1
                         | otherwise      = 0
countUses _ ExpName{}                     = 0
countUses i (ExpLambda j _ e) | i==j      = 0
                              | otherwise = countUses i e
countUses i (ExpApply e1 e2)              = ((+) `on` countUses i) e1 e2
countUses _ ExpHole{}                     = 0
countUses i (ExpLetMatch _ _ bindExp inExp) = ((+) `on` countUses i) bindExp inExp
countUses i (ExpLet _ _ bindExp inExp)    = ((+) `on` countUses i) bindExp inExp
countUses i (ExpCaseMatch bindExp alts)   = sum $ countUses i bindExp
                                                : [ countUses i expr
                                                  | (_, _, expr) <- alts
                                                  ]

