module Data.Ratio where



data Ratio a

instance Prelude.Integral a => Prelude.Enum (Ratio a)
instance Data.Data.Data.Eq.Eq a => Data.Data.Data.Eq.Eq (Ratio a)
instance Prelude.Integral a => Prelude.Fractional (Ratio a)
instance (Data.Data.Data a, Prelude.Integral a) => Data.Data.Data (Ratio a)
instance Prelude.Integral a => Prelude.Num (Ratio a)
instance Prelude.Integral a => Data.Data.Data.Ord.Ord (Ratio a)
instance (Prelude.Integral a, Text.Read.Read a) => Text.Read.Read (Ratio a)
instance Prelude.Integral a => Prelude.Real (Ratio a)
instance Prelude.Integral a => Prelude.RealFrac (Ratio a)
instance (Prelude.Integral a, Text.Show.Show a) => Text.Show.Show (Ratio a)
instance (Foreign.Storable.Storable a, Prelude.Integral a) => Foreign.Storable.Storable (Ratio a)

data Rational

instance Prelude.Enum Rational
instance Data.Data.Data.Eq.Eq Rational
instance Prelude.Fractional Rational
instance Data.Data.Data Rational
instance Prelude.Num Rational
instance Data.Data.Data.Ord.Ord Rational
instance Text.Read.Read Rational
instance Prelude.Real Rational
instance Prelude.RealFrac Rational
instance Text.Show.Show Rational
instance Foreign.Storable.Storable Rational

(%) :: Prelude.Integral a => a -> a -> Ratio a 
