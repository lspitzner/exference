module Prelude where



data Float
data Double
data Integer
data Ordering




class (Text.Show.Show a, Data.Eq.Eq a) => Num a where
  fromInteger :: Integer -> a

class (Num a, Data.Ord.Ord a) => Real a where
  toRational :: a -> Rational

class Num a => Fractional a where
  fromRational :: Rational -> a
  -- omit (/) and recip

class Enum a where
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromToThenTo :: a -> a -> a -> [a] -- all these methods might be bad for
                                         -- inference purposes
  -- omit succ, pred

class (Real a, Enum a) => Integral a where
  toInteger :: a -> Integer
  -- omit all other stuff

class (Real a, Fractional a) => RealFrac a where
  truncate :: Integral b => a -> b
  round    :: Integral b => a -> b
  ceiling  :: Integral b => a -> b
  floor    :: Integral b => a -> b
  -- omit properFraction

class Fractional a => Floating a where
  -- omit everything :D

class (RealFrac a, Floating a) => RealFloat a where
  -- omit everything.. this stuff is too obscure; i think we are better of
  -- in the general case without it.

class Bounded a where
  minBound, maxBound :: a -- fun for inference: you get an a for free!
  -- best idea probably to not declare any instances for this type class;
  -- otherwise, this will tank the inference performance.

fromIntegral :: (Integral a, Num b) => a -> b
realToFrac :: (Real a, Fractional b) => a -> b

instance Integral a => Num (Ratio a)
instance Num a => Num (Product a)
instance Num a => Num (Sum a)
instance RealFloat a => Num (Complex a)
instance HasResolution a => Num (Fixed a)

instance Integral a => Real (Ratio a)
instance HasResolution a => Real (Fixed a)

instance Integral a => RealFrac (Ratio a)
instance HasResolution a => RealFrac (Fixed a)

instance Integral a => Fractional (Ratio a)
instance RealFloat a => Fractional (Complex a)
instance HasResolution a => Fractional (Fixed a)

instance RealFloat a => Floating (Complex a)

instance Data.Eq.Eq Float
instance Floating Float
instance Data.Data.Data Float
instance Data.Ord.Ord Float
instance Text.Read.Read Float
instance RealFloat Float
instance GHC.Generics.Generic Float
instance Foreign.Storable.Storable Float
instance Text.Printf.PrintfArg Float

instance Data.Eq.Eq Double
instance Floating Double
instance Data.Data.Data Double
instance Data.Ord.Ord Double
instance Text.Read.Read Double
instance RealFloat Double
instance GHC.Generics.Generic Double
instance Foreign.Storable.Storable Double
instance Text.Printf.PrintfArg Double

instance Enum Integer
instance Data.Eq.Eq Integer
instance Integral Integer
instance Data.Data.Data Integer
instance Num Integer
instance Data.Ord.Ord Integer
instance Text.Read.Read Integer
instance Real Integer
instance Text.Show.Show Integer
instance Data.Ix.Ix Integer
instance Data.Bits.Bits Integer
instance Text.Printf.PrintfArg Integer

instance Bounded Ordering
instance Enum Ordering
instance Data.Eq.Eq Ordering
instance Data.Data.Data Ordering
instance Data.Ord.Ord Ordering
instance Text.Read.Read Ordering
instance Text.Show.Show Ordering
instance Data.Ix.Ix Ordering
instance GHC.Generics.Generic Ordering
instance Data.Monoid.Monoid Ordering
