module Data.Text where



data Text

pack :: Data.String.String -> Text
unpack :: Text -> Data.String.String



-- instance IsList Text
instance Data.Eq.Eq Text
instance Data.Data.Data Text
instance Data.Ord.Ord Text
instance Text.Read.Read Text
instance Text.Show.Show Text
-- instance IsString Text
instance Data.Monoid.Monoid Text
-- instance Binary Text
