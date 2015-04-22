module Foreign.Storable where



class Storable a where

instance (Storable a, Prelude.Integral a) => Storable (Data.Ratio.Ratio a)
-- instance Storable (StablePtr a)
-- instance Storable (Ptr a)
-- instance Storable (FunPtr a)
-- instance Storable a => Storable (Complex a)
   