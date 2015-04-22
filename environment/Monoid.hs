module Data.Monoid where



class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a


instance Monoid Ordering  
instance Monoid ()  
instance Monoid Any   
instance Monoid All   
instance Monoid Event   
instance Monoid [a]   
instance Monoid a => Monoid (Maybe a)  
instance Monoid (Last a)  
instance Monoid (First a)   
instance Prelude.Num a => Monoid (Product a)  
instance Prelude.Num a => Monoid (Sum a)  
instance Monoid (Endo a)  
instance Monoid a => Monoid (Dual a)  
-- instance Monoid b => Monoid (a -> b)  
instance (Monoid a, Monoid b) => Monoid (a, b)  
-- instance Monoid (Proxy * s)   
instance Monoid a => Monoid (Const a b)   
-- instance Typeable (* -> Constraint) Monoid  
instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c)   
instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a, b, c, d)  
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) => Monoid (a, b, c, d, e)

data Dual a
data Endo a
data All
data Any
data Sum a
data Product a
data First a
data Last a
data Alt f a
