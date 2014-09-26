module Language.Haskell.ExferenceCore
  ( findExpressions
  , findExpressionsChunked
  , E.ExferenceHeuristicsConfig (..)
  , E.ExferenceInput (..)
  , E.ExferenceOutputElement
  )
where



import qualified Language.Haskell.Exference.Internal.Exference as E



findExpressions :: E.ExferenceInput -> [E.ExferenceOutputElement]
findExpressions = concat . E.findExpressions

findExpressionsChunked :: E.ExferenceInput
                   -> [[E.ExferenceOutputElement]]
findExpressionsChunked = E.findExpressions
