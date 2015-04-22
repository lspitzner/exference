module Data.Void where



data Void

instance Data.Eq.Eq Void  
instance Data.Data.Data Void  
instance Data.Ord.Ord Void   
instance Text.Read.Read Void 
instance Text.Show.Show Void  
instance Data.Ix.Ix Void  
instance GHC.Generics.Generic Void   
-- instance Exception Void   
-- instance Hashable Void  
-- instance Semigroup Void   
-- instance Typeable * Void  

absurd :: Void -> a

vacuous :: Functor f => f Void -> f a
