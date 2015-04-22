module Data.Ord where



data Ordering

class Data.Eq.Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<)  :: a -> a -> Data.Bool.Bool
  (>=) :: a -> a -> Data.Bool.Bool
  (>)  :: a -> a -> Data.Bool.Bool
  (<=) :: a -> a -> Data.Bool.Bool

comparing :: Ord a => (b -> a) -> b -> b -> Ordering

instance Ord a => Ord [a]
instance Prelude.Integral a => Ord (Ratio a)
-- instance Ord p => Ord (Par1 p)
instance Ord a => Ord (Data.Maybe.Maybe a)
instance Ord a => Ord (Down a)
instance Ord a => Ord (Last a)
instance Ord a => Ord (First a)
instance Ord a => Ord (Product a)
instance Ord a => Ord (Sum a)
instance Ord a => Ord (Dual a)
instance Ord a => Ord (ZipList a)
instance (Ord a, Ord b) => Ord (Data.Either.Either a b)
-- instance Ord (f p) => Ord (Rec1 f p)
instance (Ord a, Ord b) => Ord (a, b)
-- instance Ord c => Ord (K1 i c p)
-- instance (Ord (f p), Ord (g p)) => Ord ((:+:) f g p)
-- instance (Ord (f p), Ord (g p)) => Ord ((:*:) f g p)
-- instance Ord (f (g p)) => Ord ((:.:) f g p)
instance (Ord a, Ord b, Ord c) => Ord (a, b, c)
-- instance Ord (f p) => Ord (M1 i c f p)
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

data Down a

instance Data.Eq.Eq a => Data.Eq.Eq (Down a)
instance Data.Ord.Ord a => Data.Ord.Ord (Down a)
instance Text.Read.Read a => Text.Read.Read (Down a)
instance Text.Show.Show a => Text.Show.Show (Down a) 
