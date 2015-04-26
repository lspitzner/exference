module Control.Category where



class Category cat where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c
