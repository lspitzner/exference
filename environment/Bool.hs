module Data.Bool where



data Bool

(&&) :: Bool -> Bool -> Bool
(||) :: Bool -> Bool -> Bool

bool :: a -> a -> Bool -> a 

instance Bounded Bool
instance Enum Bool
instance Data.Eq.Eq Bool
instance Data.Data.Data Bool
instance Data.Ord.Ord Bool
instance Text.Read.Read Bool
instance Text.Show.Show Bool
instance Data.Ix.Ix Bool
instance GHC.Generics.Generic Bool
instance Data.Bits.FiniteBits Bool
instance Data.Bits.Bits Bool
instance Foreign.Storable.Storable Bool
