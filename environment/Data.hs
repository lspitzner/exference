module Data.Data where



data DataType
data Constr
data DataRep
data ConstrRep
data Fixity

class Typeable a => Data a where
  gfoldl  :: (forall d b. Data d => c (d -> b) -> d -> c b)
          -> (forall g. g -> c g)
          -> a
          -> c a

  gunfold :: (forall b r. Data b => c (b -> r) -> c r)
          -> (forall r. r -> c r)
          -> Constr
          -> c a

  toConstr   :: a -> Constr

  dataTypeOf  :: a -> DataType

  dataCast1 :: Typeable t
            => (forall d. Data d => c (t d))
            -> Data.Maybe.Maybe (c a)

  dataCast2 :: Typeable t
            => (forall d e. (Data d, Data e) => c (t d e))
            -> Data.Maybe.Maybe (c a)

  gmapT :: (forall b. Data b => b -> b) -> a -> a

  gmapQl :: forall r r'. (r -> r' -> r) -> r -> (forall d. Data d => d -> r') -> a -> r

  gmapQr :: forall r r'. (r' -> r -> r) -> r -> (forall d. Data d => d -> r') -> a -> r

  gmapQ :: (forall d. Data d => d -> u) -> a -> [u]

  gmapQi :: forall u. Int -> (forall d. Data d => d -> u) -> a -> u

  gmapM :: forall m. Control.Monad.Monad m => (forall d. Data d => d -> m d) -> a -> m a

  gmapMp :: forall m. Control.Monad.MonadPlus m => (forall d. Data d => d -> m d) -> a -> m a

  gmapMo :: forall m. Control.Monad.MonadPlus m => (forall d. Data d => d -> m d) -> a -> m a


instance Data Data.Bool.Bool
instance Data Data.Char.Char
instance Data Prelude.Double
instance Data Prelude.Float
instance Data Data.Int.Int
instance Data Data.Int.Int8
instance Data Data.Int.Int16
instance Data Data.Int.Int32
instance Data Data.Int.Int64
instance Data Prelude.Integer
instance Data Data.Ord.Ordering
instance Data Data.Word.Word
instance Data Data.Word.Word8
instance Data Data.Word.Word16
instance Data Data.Word.Word32
instance Data Data.Word.Word64
instance Data ()
-- instance Data Version
-- instance Data Natural
-- instance Data SpecConstrAnnotation
instance Data Data.Void.Void
instance Data a => Data [a]
-- instance (Data a, Prelude.Integral a) => Data (Ratio a)
-- instance (Data a, Typeable * a) => Data (Ptr a)
instance Data a => Data (Data.Maybe.Maybe a)
-- instance (Data a, Typeable * a) => Data (ForeignPtr a)
-- instance Data a => Data (Complex a)
-- instance Typeable * a => Data (Fixed a)
-- instance Data a => Data (Identity a)
instance (Data a, Data b) => Data (Data.Either.Either a b)
instance (Data a, Data b) => Data (a, b)
-- instance Data t => Data (Proxy * t)
instance (Data a, Data b, Data c) => Data (a, b, c)
-- instance ((~) * a b, Data a) => Data ((:~:) * a b)
-- instance (Coercible * a b, Data a, Data b) => Data (Coercion * a b)
instance (Data a, Data b, Data c, Data d) => Data (a, b, c, d)
instance (Data a, Data b, Data c, Data d, Data e) => Data (a, b, c, d, e)
instance (Data a, Data b, Data c, Data d, Data e, Data f) => Data (a, b, c, d, e, f)
instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g) => Data (a, b, c, d, e, f, g)
