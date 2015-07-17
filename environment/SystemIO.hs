module System.IO where



data IO a

instance Data.Functor.Functor IO
instance Control.Applicative.Applicative IO
instance Control.Monad.Monad IO

type FilePath = Data.String.String
