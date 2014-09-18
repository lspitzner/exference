module ExferenceDict where



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
-- **** plain old function signatures ****
-- ****

-- general rules:
-- 1) don't add too polymorphic stuff
-- 2) don't add f if g is already added and f = flip g

maybe :: b -> (a -> b) -> Maybe a -> b
either :: (a->c) -> (b->c) -> Either a b -> c
curry :: ((a, b) -> c) -> a -> b -> c
uncurry :: (a -> b -> c) -> ((a, b) -> c)
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

-- ****
-- **** type class instances ****
-- ****

{- TODO:
Show a
Functor f
Applicative f
Monad m
MonadState s m | m -> s
Eq a
Num a
Ord a
Real a
Fractional a
Enum a
Integral a
RealFrac a
Floating a
RealFloat a
Bounded a
Ix a
MonadPlus m
-}
