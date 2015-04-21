module Text.Show where



data ShowS

class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> Data.String.String
  showList :: [a] -> ShowS

instance Show Char
instance Show Double
instance Show Float
instance Show Int
instance Show Int8
instance Show Int16
instance Show Int32
instance Show Int64
instance Show Integer
instance Show Ordering
instance Show Word
instance Show Word8
instance Show Word16
instance Show Word32
instance Show Word64
instance Show ()
instance Show SomeException
instance Show Number
instance Show Lexeme
instance Show Fingerprint
instance Show TyCon
instance Show TypeRep
instance Show Associativity
instance Show Fixity
instance Show Arity
instance Show Any
instance Show All
instance Show ArithException
instance Show ErrorCall
instance Show IOException
instance Show MaskingState
instance Show Dynamic
instance Show CUIntMax
instance Show CIntMax
instance Show CUIntPtr
instance Show CIntPtr
instance Show CSUSeconds
instance Show CUSeconds
instance Show CTime
instance Show CClock
instance Show CSigAtomic
instance Show CWchar
instance Show CSize
instance Show CPtrdiff
instance Show CDouble
instance Show CFloat
instance Show CULLong
instance Show CLLong
instance Show CULong
instance Show CLong
instance Show CUInt
instance Show CInt
instance Show CUShort
instance Show CShort
instance Show CUChar
instance Show CSChar
instance Show CChar
instance Show GeneralCategory
instance Show IntPtr
instance Show WordPtr
instance Show CodingProgress
instance Show TextEncoding
instance Show SeekMode
instance Show NewlineMode
instance Show Newline
instance Show BufferMode
instance Show Handle
instance Show IOErrorType
instance Show ExitCode
instance Show ArrayException
instance Show AsyncException
instance Show SomeAsyncException
instance Show AssertionFailed
instance Show Deadlock
instance Show BlockedIndefinitelyOnSTM
instance Show BlockedIndefinitelyOnMVar
instance Show Fd
instance Show CRLim
instance Show CTcflag
instance Show CSpeed
instance Show CCc
instance Show CUid
instance Show CNlink
instance Show CGid
instance Show CSsize
instance Show CPid
instance Show COff
instance Show CMode
instance Show CIno
instance Show CDev
instance Show CodingFailureMode
instance Show ThreadStatus
instance Show BlockReason
instance Show ThreadId
instance Show NestedAtomically
instance Show NonTermination
instance Show NoMethodError
instance Show RecUpdError
instance Show RecConError
instance Show RecSelError
instance Show PatternMatchFail
instance Show IOMode
instance Show Event
instance Show FdKey
instance Show HandlePosn
instance Show TyCon
instance Show TypeRep   
instance Show SomeSymbol  
instance Show SomeNat   
instance Show Version   
instance Show Fixity  
instance Show ConstrRep   
instance Show DataRep   
instance Show Constr  
instance Show DataType  
instance Show GCStats   
instance Show a => Show [a]   
instance (Integral a, Show a) => Show (Ratio a)   
instance Show (Ptr a)   
instance Show (FunPtr a)  
instance Show (U1 p)  
instance Show p => Show (Par1 p)
instance Show a => Show (Maybe a)
instance Show a => Show (Down a)
instance Show a => Show (Last a)
instance Show a => Show (First a)
instance Show a => Show (Product a)
instance Show a => Show (Sum a)
instance Show a => Show (Dual a)
instance Show (ForeignPtr a)
instance Show a => Show (ZipList a)
instance Show a => Show (Complex a)
instance HasResolution a => Show (Fixed a)
instance Show (a -> b)
instance (Show a, Show b) => Show (Data.Either.Either a b)
instance Show (f p) => Show (Rec1 f p)
instance (Show a, Show b) => Show (a, b)
instance Show (ST s a)
instance Show (Proxy k s)
instance Show c => Show (K1 i c p)
instance (Show (f p), Show (g p)) => Show ((:+:) f g p)
instance (Show (f p), Show (g p)) => Show ((:*:) f g p)
instance Show (f (g p)) => Show ((:.:) f g p)
instance (Show a, Show b, Show c) => Show (a, b, c)
instance Show ((:~:) k a b)
instance Show (Coercion k a b)
instance Show (f p) => Show (M1 i c f p)
instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d)
instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e)
instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a, b, c, d, e, f)
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Show (a, b, c, d, e, f, g)
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) => Show (a, b, c, d, e, f, g, h)
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i) => Show (a, b, c, d, e, f, g, h, i)
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j) => Show (a, b, c, d, e, f, g, h, i, j)
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k) => Show (a, b, c, d, e, f, g, h, i, j, k)
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l) => Show (a, b, c, d, e, f, g, h, i, j, k, l)
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m) => Show (a, b, c, d, e, f, g, h, i, j, k, l, m)
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n) => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o) => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
