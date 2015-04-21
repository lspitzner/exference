module Data.Ix where



class Ord a => Ix a where
  range :: (a, a) -> [a]
  index :: (a, a) -> a -> Int
  inRange :: (a, a) -> a -> Bool
  rangeSize :: (a, a) -> Int
  -- same restriction as for Bounded: better don't define instances for now.

-- instance Ix Bool
-- instance Ix Char
-- instance Ix Int
-- instance Ix Int8
-- instance Ix Int16
-- instance Ix Int32
-- instance Ix Int64
-- instance Ix Integer
-- instance Ix Ordering
-- instance Ix Word
-- instance Ix Word8
-- instance Ix Word16
-- instance Ix Word32
-- instance Ix Word64
-- instance Ix ()
-- instance Ix GeneralCategory
-- instance Ix SeekMode
-- instance Ix IOMode
-- instance Ix Natural
-- instance Ix Void
-- instance (Ix a, Ix b) => Ix (a, b)
-- instance Ix (Proxy k s)
-- instance (Ix a1, Ix a2, Ix a3) => Ix (a1, a2, a3)
-- instance (Ix a1, Ix a2, Ix a3, Ix a4) => Ix (a1, a2, a3, a4)
-- instance (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5) => Ix (a1, a2, a3, a4, a5)
   