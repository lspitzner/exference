module GHC.Generics where



data V1 p
data U1 p
data Par1 p
data Rec1 f p 
data K1 i c p 
data M1 i c f p 

class Generic a where
class Generic1 f where

instance Generic (a, b)
instance Generic (a, b, c)
instance Generic (a, b, c, d)
instance Generic (a, b, c, d, e)
instance Generic (a, b, c, d, e, f)
instance Generic (a, b, c, d, e, f, g)
