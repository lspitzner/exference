module Data.Typeable where



data TypeRep

instance Data.Eq.Eq TypeRep
instance Data.Ord.Ord TypeRep
instance Text.Show.Show TypeRep

class Typeable a where

typeOf  :: forall a. Typeable a => a -> TypeRep
typeOf1 :: forall t a. Typeable t => t a -> TypeRep
typeOf2 :: forall t a b. Typeable t => t a b -> TypeRep
typeOf3 :: forall t a b c. Typeable t => t a b c -> TypeRep
typeOf4 :: forall t a b c d. Typeable t => t a b c d -> TypeRep
typeOf5 :: forall t a b c d e. Typeable t => t a b c d e -> TypeRep
typeOf6 :: forall t a b c d e f. Typeable t => t a b c d e f -> TypeRep
typeOf7 :: forall t a b c d e f g. Typeable t => t a b c d e f g -> TypeRep

cast :: forall a b. (Typeable a, Typeable b) => a -> Maybe b 
