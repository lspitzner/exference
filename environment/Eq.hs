module Data.Eq where



class Eq a where
  (==) :: a -> a -> Data.Bool.Bool
  (/=) :: a -> a -> Data.Bool.Bool

instance Eq Char
instance Eq Double
instance Eq Float
instance Eq Int
instance Eq Int8
instance Eq Int16
instance Eq Int32
instance Eq Int64
instance Eq Integer
instance Eq Ordering
instance Eq Word
instance Eq Word8
instance Eq Word16
instance Eq Word32
instance Eq Word64
instance Eq ()
instance Eq Number
instance Eq Lexeme
instance Eq Fingerprint
instance Eq TyCon
instance Eq TypeRep
instance Eq Associativity
instance Eq Fixity
instance Eq Arity
instance Eq Any
instance Eq All
instance Eq ArithException
instance Eq ErrorCall
instance Eq IOException
instance Eq MaskingState
instance Eq CUIntMax
instance Eq CIntMax
instance Eq CUIntPtr
instance Eq CIntPtr
instance Eq CSUSeconds
instance Eq CUSeconds
instance Eq CTime
instance Eq CClock
instance Eq CSigAtomic
instance Eq CWchar
instance Eq CSize
instance Eq CPtrdiff
instance Eq CDouble
instance Eq CFloat
instance Eq CULLong
instance Eq CLLong
instance Eq CULong
instance Eq CLong
instance Eq CUInt
instance Eq CInt
instance Eq CUShort
instance Eq CShort
instance Eq CUChar
instance Eq CSChar
instance Eq CChar
instance Eq GeneralCategory
instance Eq IntPtr
instance Eq WordPtr
instance Eq BufferState
instance Eq CodingProgress
instance Eq SeekMode
instance Eq IODeviceType
instance Eq NewlineMode
instance Eq Newline
instance Eq BufferMode
instance Eq Handle
instance Eq IOErrorType
instance Eq ExitCode
instance Eq ArrayException
instance Eq AsyncException
instance Eq Errno
instance Eq Fd
instance Eq CRLim
instance Eq CTcflag
instance Eq CSpeed
instance Eq CCc
instance Eq CUid
instance Eq CNlink
instance Eq CGid
instance Eq CSsize
instance Eq CPid
instance Eq COff
instance Eq CMode
instance Eq CIno
instance Eq CDev
instance Eq ThreadStatus
instance Eq BlockReason
instance Eq ThreadId
instance Eq IOMode
instance Eq Event
instance Eq FdKey
instance Eq TimeoutKey
instance Eq HandlePosn
instance Eq TyCon
instance Eq TypeRep
instance Eq TypeRepKey
instance Eq SomeSymbol
instance Eq SomeNat
instance Eq Version
instance Eq Fixity
instance Eq ConstrRep
instance Eq DataRep
instance Eq Constr
instance Eq Unique
instance Eq SpecConstrAnnotation
instance Eq a => Eq [a]
instance Eq a => Eq (Ratio a)
instance Eq (StablePtr a)
instance Eq (Ptr a)
instance Eq (FunPtr a)
instance Eq (U1 p)
instance Eq p => Eq (Par1 p)
instance Eq a => Eq (Maybe a)
instance Eq a => Eq (Down a)
instance Eq a => Eq (Last a)
instance Eq a => Eq (First a)
instance Eq a => Eq (Product a)
instance Eq a => Eq (Sum a)
instance Eq a => Eq (Dual a)
instance Eq (IORef a)
instance Eq (MVar a)
instance Eq (ForeignPtr a)
instance Eq (TVar a)
instance Eq (Chan a)
instance Eq a => Eq (ZipList a)
instance Eq a => Eq (Complex a)
instance Eq (Fixed a)
instance Eq (StableName a)
instance (Eq a, Eq b) => Eq (Data.Either.Either a b)
instance Eq (f p) => Eq (Rec1 f p)
instance (Eq a, Eq b) => Eq (a, b)
instance Eq (STRef s a)
instance Eq (Proxy k s)
instance Eq c => Eq (K1 i c p)
--instance (Eq (f p), Eq (g p)) => Eq ((:+:) f g p)
--instance (Eq (f p), Eq (g p)) => Eq ((:*:) f g p)
instance Eq (f (g p)) => Eq ((:.:) f g p)
instance (Eq a, Eq b, Eq c) => Eq (a, b, c)
-- instance Eq ((:~:) k a b)
instance Eq (Coercion k a b)
instance Eq (f p) => Eq (M1 i c f p)
instance (Eq a, Eq b, Eq c, Eq d) => Eq (a, b, c, d)
instance (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (a, b, c, d, e)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => Eq (a, b, c, d, e, f)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => Eq (a, b, c, d, e, f, g)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) => Eq (a, b, c, d, e, f, g, h)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) => Eq (a, b, c, d, e, f, g, h, i)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) => Eq (a, b, c, d, e, f, g, h, i, j)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) => Eq (a, b, c, d, e, f, g, h, i, j, k)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l) => Eq (a, b, c, d, e, f, g, h, i, j, k, l)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m) => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n) => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o) => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
