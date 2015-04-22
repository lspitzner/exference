module Data.Map where



data Map k a

lookup :: Data.Ord.Ord k => k -> Map k a -> Data.Maybe.Maybe a
