module ExferenceDict where


runEitherT :: EitherT e m a -> m (Either e a)

instance Monad m => Monad (EitherT e m)

-- this one is waay to polymorphic for practical purposes, apparently..
-- mapEitherT :: (m (Either e a) -> n (Either e' b)) -> EitherT e m a -> EitherT e' n b 

-- instance Functor (EitherT e m)

-- ****
-- **** type classes ****
-- ****

class Show a where
  showsPrec :: Int -> a -> ShowS
  show      :: a   -> String
  showList  :: [a] -> ShowS

class Functor f where
  fmap :: (a->b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a->b) -> f a -> f b

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b

class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()
  state :: (s -> (a,s)) -> m a

class Eq a where -- don't list methods, because (==) and (/=) are baaaaad
                 -- (inference-wise..)

class (Show a, Eq a) => Num a where
  fromInteger :: Integer -> a

class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<)  :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  (>)  :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  -- omit max, min; too polymorphic for inference

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

class Ord a => Ix a where
  range :: (a, a) -> [a]
  index :: (a, a) -> a -> Int
  inRange :: (a, a) -> a -> Bool
  rangeSize :: (a, a) -> Int
  -- same restriction as for Bounded: better don't define instances for now.

class Monad m => MonadPlus m where
  -- mzero :: m a -- this is a critical case: too generic, yet we might need it
                  -- at times
  mplus :: m a -> m a -> m a

-- ****
-- **** data types (constructors and deconstructors aka pattern matching)
-- ****

newtype State s a = State (s -> (a,s))

newtype Cont r a = Cont ((a -> r) -> r)

instance Monad (State s)

data Maybe a = Just a
             | Nothing

data Either a b = Left a
                | Right b

-- ****
-- **** plain old function signatures ****
-- ****

-- general rules:
-- 1) don't add too polymorphic stuff
-- 2) don't add f if g is already added and f = flip g

maybe :: b -> (a -> b) -> Maybe a -> b
-- fromMaybe :: a -> Maybe a -> a
either :: (a->c) -> (b->c) -> Either a b -> c
-- these two are not really necessary and confuse the algorithm for quad
{-
curry :: ((a, b) -> c) -> a -> b -> c
uncurry :: (a -> b -> c) -> ((a, b) -> c)
-}
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- no forM, see rule 2
sequence :: Monad m => [m a] -> m [a] -- Traversable version better?
foldl :: (a -> b -> a) -> a -> [b] -> a
foldr :: (a -> b -> b) -> b -> [a] -> b
concat :: [[a]] -> [a]
zip :: [a] -> [b] -> [(a, b)]
zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
unzip :: [(a, b)] -> ([a], [b])
unzip3 :: [(a, b, c)] -> ([a], [b], [c])
(&&) :: Bool -> Bool -> Bool
(||) :: Bool -> Bool -> Bool

-- from Control.Monad
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
join :: Monad m => m (m a) -> m a 
msum :: MonadPlus m => [m a] -> m a 
mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a 
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
liftA2 :: Applicative f => (a1 -> a2 -> r) -> f a1 -> f a2 -> f r
liftA3 :: Applicative f => (a1 -> a2 -> a3 -> r) -> f a1 -> f a2 -> f a3 -> f r 
liftA4 :: Applicative f => (a1 -> a2 -> a3 -> a4 -> r) -> f a1 -> f a2 -> f a3 -> f a4 -> f r 
liftA5 :: Applicative f => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> f a1 -> f a2 -> f a3 -> f a4 -> f a5 -> f r 

realToFrac :: (Real a, Fractional b) => a -> b

mapLookup :: Ord k => k -> Map k a -> Maybe a

-- ****
-- **** type class instances ****
-- ****

{- done from hoogle info:
Show a
Functor f
Applicative f
Monad m
MonadState s m | m -> s
Eq a
Num a
Ord a
MonadPlus m
Real a
Integral a
-}

{- TODO:
Enum a
Bounded a
Ix a
-}

instance Functor []
instance Functor IO
instance Functor Maybe
instance Functor ReadP
instance Functor ReadPrec
instance Functor STM
instance Functor Handler
instance Functor ZipList
instance Functor ArgDescr
instance Functor OptDescr
instance Functor ArgOrder
instance Functor ((->) r)
instance Functor (Either a)
instance Functor ((,) a)
instance Functor (ST s)
-- instance Functor (Proxy *)
instance Arrow a => Functor (ArrowMonad a)
instance Functor (ST s)
instance Monad m => Functor (WrappedMonad m)
instance Functor (Const m)
instance Arrow a => Functor (WrappedArrow a b)

instance Applicative []
instance Applicative IO
instance Applicative Maybe
instance Applicative ReadP
instance Applicative ReadPrec
instance Applicative STM
instance Applicative ZipList
instance Applicative ((->) a)
instance Applicative (Either e)
instance Monoid a => Applicative ((,) a)
instance Applicative (ST s)
-- instance Applicative (Proxy *)
instance Arrow a => Applicative (ArrowMonad a)
instance Applicative (ST s)
instance Monad m => Applicative (WrappedMonad m)
instance Monoid m => Applicative (Const m)
instance Arrow a => Applicative (WrappedArrow a b)
-- instance Typeable ((* -> *) -> Constraint) Applicative

instance Monad []
instance Monad IO
instance Monad Maybe
instance Monad ReadP
instance Monad ReadPrec
instance Monad STM
instance Monad ((->) r)
instance Monad (Either e)
instance Monad (ST s)
-- instance Monad (Proxy *)
instance ArrowApply a => Monad (ArrowMonad a)
instance Monad (ST s)
instance Monad m => Monad (WrappedMonad m)

instance MonadState s m => MonadState s (MaybeT m)
instance MonadState s m => MonadState s (ListT m)
instance MonadState s m => MonadState s (IdentityT m)
instance (Monoid w, MonadState s m) => MonadState s (WriterT w m)
instance (Monoid w, MonadState s m) => MonadState s (WriterT w m)
instance MonadState s m => MonadState s (ReaderT r m)
instance MonadState s m => MonadState s (ExceptT e m)
instance (Error e, MonadState s m) => MonadState s (ErrorT e m)
instance MonadState s m => MonadState s (ContT r m)
instance Monad m => MonadState s (StateT s m)
instance Monad m => MonadState s (StateT s m)
instance (Monad m, Monoid w) => MonadState s (RWST r w s m)
instance (Monad m, Monoid w) => MonadState s (RWST r w s m)

instance MonadPlus []
instance MonadPlus Maybe
instance MonadPlus ReadP
instance MonadPlus ReadPrec
instance MonadPlus STM
instance (ArrowApply a, ArrowPlus a) => MonadPlus (ArrowMonad a)

instance Show Bool
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
instance (Show a, Show b) => Show (Either a b)
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

instance Eq Bool
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
instance (Eq a, Eq b) => Eq (Either a b)
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

instance Ord Bool
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
instance (Ord a, Ord b) => Ord (Either a b)
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
