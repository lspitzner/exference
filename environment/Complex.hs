module Data.Complex where



data Complex a

instance Data.Eq.Eq a => Data.Eq.Eq (Complex a)   
instance Prelude.RealFloat a => Floating (Complex a)   
instance Prelude.RealFloat a => Fractional (Complex a)   
instance Data.Data.Data a => Data.Data.Data (Complex a)   
instance Prelude.RealFloat a => Prelude.Num (Complex a)   
instance Text.Read.Read a => Text.Read.Read (Complex a)   
instance Text.Show.Show a => Text.Show.Show (Complex a)   
instance Foreign.Storable.Storable a => Foreign.Storable.Storable (Complex a)   

realPart :: Complex a -> a
imagPart :: Complex a -> a
