module Text.Show where



data ShowS

class Show a where
  showsPrec :: Data.Int.Int -> a -> ShowS
  show :: a -> Data.String.String
  showList :: [a] -> ShowS

instance Show a => Show [a]   
instance (Prelude.Integral a, Show a) => Show (Data.Ratio.Ratio a)   
-- instance Show p => Show (Par1 p)
instance Show a => Show (Data.Maybe.Maybe a)
instance Show a => Show (Data.Monoid.Down a)
instance Show a => Show (Data.Monoid.Last a)
instance Show a => Show (Data.Monoid.First a)
instance Show a => Show (Data.Monoid.Product a)
instance Show a => Show (Data.Monoid.Sum a)
instance Show a => Show (Data.Monoid.Dual a)
instance Show a => Show (Control.Applicative.ZipList a)
-- instance Show a => Show (Complex a)
-- instance HasResolution a => Show (Fixed a)
-- instance Show (a -> b)
instance (Show a, Show b) => Show (Data.Either.Either a b)
-- instance Show (f p) => Show (Rec1 f p)
instance (Show a, Show b) => Show (a, b)
-- instance Show c => Show (K1 i c p)
-- instance (Show (f p), Show (g p)) => Show ((:+:) f g p)
-- instance (Show (f p), Show (g p)) => Show ((:*:) f g p)
-- instance Show (f (g p)) => Show ((:.:) f g p)
instance (Show a, Show b, Show c) => Show (a, b, c)
-- instance Show (f p) => Show (M1 i c f p)
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
