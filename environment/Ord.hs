module Data.Ord where



data Ordering

class Data.Eq.Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<)  :: a -> a -> Data.Bool.Bool
  (>=) :: a -> a -> Data.Bool.Bool
  (>)  :: a -> a -> Data.Bool.Bool
  (<=) :: a -> a -> Data.Bool.Bool

comparing :: Ord a => (b -> a) -> b -> b -> Ordering

instance Ord Char
instance Ord Double
instance Ord Float
instance Ord Int
instance Ord Int8
instance Ord Int16
instance Ord Int32
instance Ord Int64
instance Ord Integer
instance Ord Ordering
instance Ord Word
instance Ord Word8
instance Ord Word16
instance Ord Word32
instance Ord Word64
instance Ord ()
instance Ord Fingerprint
instance Ord TyCon
instance Ord TypeRep
instance Ord Associativity
instance Ord Fixity
instance Ord Arity
instance Ord Any
instance Ord All
instance Ord ArithException
instance Ord ErrorCall
instance Ord CUIntMax
instance Ord CIntMax
instance Ord CUIntPtr
instance Ord CIntPtr
instance Ord CSUSeconds
instance Ord CUSeconds
instance Ord CTime
instance Ord CClock
instance Ord CSigAtomic
instance Ord CWchar
instance Ord CSize
instance Ord CPtrdiff
instance Ord CDouble
instance Ord CFloat
instance Ord CULLong
instance Ord CLLong
instance Ord CULong
instance Ord CLong
instance Ord CUInt
instance Ord CInt
instance Ord CUShort
instance Ord CShort
instance Ord CUChar
instance Ord CSChar
instance Ord CChar
instance Ord GeneralCategory
instance Ord IntPtr
instance Ord WordPtr
instance Ord SeekMode
instance Ord NewlineMode
instance Ord Newline
instance Ord BufferMode
instance Ord ExitCode
instance Ord ArrayException
instance Ord AsyncException
instance Ord Fd
instance Ord CRLim
instance Ord CTcflag
instance Ord CSpeed
instance Ord CCc
instance Ord CUid
instance Ord CNlink
instance Ord CGid
instance Ord CSsize
instance Ord CPid
instance Ord COff
instance Ord CMode
instance Ord CIno
instance Ord CDev
instance Ord ThreadStatus
instance Ord BlockReason
instance Ord ThreadId
instance Ord IOMode
instance Ord TyCon
instance Ord TypeRep
instance Ord TypeRepKey
instance Ord SomeSymbol
instance Ord SomeNat
instance Ord Version
instance Ord Unique
instance Ord a => Ord [a]
instance Integral a => Ord (Ratio a)
instance Ord (Ptr a)
instance Ord (FunPtr a)
instance Ord (U1 p)
instance Ord p => Ord (Par1 p)
instance Ord a => Ord (Maybe a)
instance Ord a => Ord (Down a)
instance Ord a => Ord (Last a)
instance Ord a => Ord (First a)
instance Ord a => Ord (Product a)
instance Ord a => Ord (Sum a)
instance Ord a => Ord (Dual a)
instance Ord (ForeignPtr a)
instance Ord a => Ord (ZipList a)
instance Ord (Fixed a)
instance (Ord a, Ord b) => Ord (Data.Either.Either a b)
instance Ord (f p) => Ord (Rec1 f p)
instance (Ord a, Ord b) => Ord (a, b)
instance Ord (Proxy k s)
instance Ord c => Ord (K1 i c p)
instance (Ord (f p), Ord (g p)) => Ord ((:+:) f g p)
instance (Ord (f p), Ord (g p)) => Ord ((:*:) f g p)
instance Ord (f (g p)) => Ord ((:.:) f g p)
instance (Ord a, Ord b, Ord c) => Ord (a, b, c)
instance Ord ((:~:) k a b)
instance Ord (Coercion k a b)
instance Ord (f p) => Ord (M1 i c f p)
instance (Ord a, Ord b, Ord c, Ord d) => Ord (a, b, c, d)
instance (Ord a, Ord b, Ord c, Ord d, Ord e) => Ord (a, b, c, d, e)
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f) => Ord (a, b, c, d, e, f)
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g) => Ord (a, b, c, d, e, f, g)
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h) => Ord (a, b, c, d, e, f, g, h)
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i) => Ord (a, b, c, d, e, f, g, h, i)
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j) => Ord (a, b, c, d, e, f, g, h, i, j)
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k) => Ord (a, b, c, d, e, f, g, h, i, j, k)
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l) => Ord (a, b, c, d, e, f, g, h, i, j, k, l)
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l, Ord m) => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m)
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n) => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n, Ord o) => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
