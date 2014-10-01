module Language.Haskell.ExferenceCore
  ( findExpressions
  , findExpressionsChunked
  , findExpressionsWithStats
  , findExpressionsPar
  , findExpressionsChunkedPar
  , findExpressionsWithStatsPar
  , E.ExferenceHeuristicsConfig (..)
  , E.ExferenceInput (..)
  , E.ExferenceOutputElement
  )
where



import qualified Language.Haskell.Exference.Internal.Exference as E
import qualified Language.Haskell.Exference.ExferenceStats as S
import qualified ListT
import Control.Monad ( join )
import Data.Monoid ( mempty )


findExpressions :: E.ExferenceInput -> [E.ExferenceOutputElement]
findExpressions = concat . map snd . E.findExpressions

findExpressionsChunked :: E.ExferenceInput
                   -> [[E.ExferenceOutputElement]]
findExpressionsChunked = map snd . E.findExpressions

findExpressionsWithStats :: E.ExferenceInput
                         -> [(S.BindingUsages, [E.ExferenceOutputElement])]
findExpressionsWithStats = E.findExpressions

findExpressionsPar :: E.ExferenceInput
                   -> (ListT.ListT IO E.ExferenceOutputElement
                       -> IO a)
                   -> IO a
findExpressionsPar input reducer =
  E.findExpressionsPar input $
    reducer . (>>= foldr ListT.cons mempty) . fmap snd

findExpressionsChunkedPar :: E.ExferenceInput
                          -> (ListT.ListT IO [E.ExferenceOutputElement]
                              -> IO a)
                          -> IO a
findExpressionsChunkedPar input reducer =
  E.findExpressionsPar input $ reducer . fmap snd

findExpressionsWithStatsPar
  :: E.ExferenceInput
  -> (   ListT.ListT IO E.ExferenceChunkElement
      -> IO a)
  -> IO a
findExpressionsWithStatsPar = E.findExpressionsPar
