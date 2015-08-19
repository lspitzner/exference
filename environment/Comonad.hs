module Control.Comonad where



class Data.Functor.Functor w => Comonad w where
  -- loses information and causes search space enlarging.
  -- extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b

liftW :: Comonad w => (a -> b) -> w a -> w b

class Comonad w => ComonadApply w where
  (<@>) :: w (a -> b) -> w a -> w b
  (@>) :: w a -> w b -> w b
  (<@) :: w a -> w b -> w a

liftW2 :: ComonadApply w => (a -> b -> c) -> w a -> w b -> w c
liftW3 :: ComonadApply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
