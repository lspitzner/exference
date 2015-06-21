module Language.Haskell.Exference.Core
  ( findExpressions
  , findExpressionsChunked
  , findExpressionsWithStats
  , E.ExferenceHeuristicsConfig (..)
  , E.ExferenceInput (..)
  , E.ExferenceOutputElement
  )
where



import qualified Language.Haskell.Exference.Core.Internal.Exference as E
import qualified Language.Haskell.Exference.Core.ExferenceStats as S
import qualified Language.Haskell.Exference.Core.SearchTree as ST
import qualified ListT
import Control.Monad ( join )
import Data.Monoid ( mempty )



findExpressions :: E.ExferenceInput -> [E.ExferenceOutputElement]
findExpressions = concatMap (\(_,_,x) -> x) . E.findExpressions

findExpressionsChunked :: E.ExferenceInput
                   -> [[E.ExferenceOutputElement]]
findExpressionsChunked = map (\(_,_,x) -> x) . E.findExpressions

findExpressionsWithStats :: E.ExferenceInput
                         -> [(S.BindingUsages, ST.SearchTree, [E.ExferenceOutputElement])]
findExpressionsWithStats = E.findExpressions
