module Prelude where



data Float
data Double
data Integer
data Ordering




class (Show a, Eq a) => Num a where
  fromInteger :: Integer -> a

class (Num a, Ord a) => Real a where
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


instance Num Double
instance Num Float
instance Num Int
instance Num Int8
instance Num Int16
instance Num Int32
instance Num Int64
instance Num Integer
instance Num Word
instance Num Word8
instance Num Word16
instance Num Word32
instance Num Word64
instance Num CUIntMax
instance Num CIntMax
instance Num CUIntPtr
instance Num CIntPtr
instance Num CSUSeconds
instance Num CUSeconds
instance Num CTime
instance Num CClock
instance Num CSigAtomic
instance Num CWchar
instance Num CSize
instance Num CPtrdiff
instance Num CDouble
instance Num CFloat
instance Num CULLong
instance Num CLLong
instance Num CULong
instance Num CLong
instance Num CUInt
instance Num CInt
instance Num CUShort
instance Num CShort
instance Num CUChar
instance Num CSChar
instance Num CChar
instance Num IntPtr
instance Num WordPtr
instance Num Fd
instance Num CRLim
instance Num CTcflag
instance Num CSpeed
instance Num CCc
instance Num CUid
instance Num CNlink
instance Num CGid
instance Num CSsize
instance Num CPid
instance Num COff
instance Num CMode
instance Num CIno
instance Num CDev
instance Integral a => Num (Ratio a)
instance Num a => Num (Product a)
instance Num a => Num (Sum a)
instance RealFloat a => Num (Complex a)
instance HasResolution a => Num (Fixed a)


instance Integral Int
instance Integral Int8
instance Integral Int16
instance Integral Int32
instance Integral Int64
instance Integral Integer
instance Integral Word
instance Integral Word8
instance Integral Word16
instance Integral Word32
instance Integral Word64
instance Integral CUIntMax
instance Integral CIntMax
instance Integral CUIntPtr
instance Integral CIntPtr
instance Integral CSigAtomic
instance Integral CWchar
instance Integral CSize
instance Integral CPtrdiff
instance Integral CULLong
instance Integral CLLong
instance Integral CULong
instance Integral CLong
instance Integral CUInt
instance Integral CInt
instance Integral CUShort
instance Integral CShort
instance Integral CUChar
instance Integral CSChar
instance Integral CChar
instance Integral IntPtr
instance Integral WordPtr
instance Integral Fd
instance Integral CRLim
instance Integral CTcflag
instance Integral CUid
instance Integral CNlink
instance Integral CGid
instance Integral CSsize
instance Integral CPid
instance Integral COff
instance Integral CMode
instance Integral CIno
instance Integral CDev

instance Real Double
instance Real Float
instance Real Int
instance Real Int8
instance Real Int16
instance Real Int32
instance Real Int64
instance Real Integer
instance Real Word
instance Real Word8
instance Real Word16
instance Real Word32
instance Real Word64
instance Real CUIntMax
instance Real CIntMax
instance Real CUIntPtr
instance Real CIntPtr
instance Real CSUSeconds
instance Real CUSeconds
instance Real CTime
instance Real CClock
instance Real CSigAtomic
instance Real CWchar
instance Real CSize
instance Real CPtrdiff
instance Real CDouble
instance Real CFloat
instance Real CULLong
instance Real CLLong
instance Real CULong
instance Real CLong
instance Real CUInt
instance Real CInt
instance Real CUShort
instance Real CShort
instance Real CUChar
instance Real CSChar
instance Real CChar
instance Real IntPtr
instance Real WordPtr
instance Real Fd
instance Real CRLim
instance Real CTcflag
instance Real CSpeed
instance Real CCc
instance Real CUid
instance Real CNlink
instance Real CGid
instance Real CSsize
instance Real CPid
instance Real COff
instance Real CMode
instance Real CIno
instance Real CDev
instance Integral a => Real (Ratio a)
instance HasResolution a => Real (Fixed a)


instance RealFrac Double
instance RealFrac Float
instance RealFrac CDouble
instance RealFrac CFloat
instance Integral a => RealFrac (Ratio a)
instance HasResolution a => RealFrac (Fixed a)


instance Fractional Double
instance Fractional Float
instance Fractional CDouble
instance Fractional CFloat
instance Integral a => Fractional (Ratio a)
instance RealFloat a => Fractional (Complex a)
instance HasResolution a => Fractional (Fixed a)


instance Floating Double
instance Floating Float
instance Floating CDouble
instance Floating CFloat
instance RealFloat a => Floating (Complex a)


instance RealFloat Double
instance RealFloat Float
instance RealFloat CDouble
instance RealFloat CFloat



