module Lecture03 where -- nazwa modułu, który piszemy


import Data.Foldable
import Data.Monoid
data List a = Empty | Cons a (List a)

x = Cons 1 (Cons 2 Empty)



instance Show a => Show (List a) where 
  show Empty = ""
  show (Cons x xs) = show x ++ "," ++ show xs


instance (Eq a) => Eq (List a) where 
-- (==) :: List a -> List  a -> Bool
 Empty == Empty = True
 (Cons x xs) == (Cons y ys) = (x == y) && (xs == ys)
 _ == _ = False
-- 
instance Functor List where
  ---fmap :: (a -> b) -> (List a) -> (List b)
  fmap f Empty       = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs) 

instance Semigroup (List a) where
  -- (<>) :: List a -> List a -> List a
  Empty <> x       = x
  (Cons x xs) <> y = Cons x (xs <> y)

instance Monoid (List a) where
  mempty = Empty
 -- mempty :: List a



-- Cwiczenie:  napisac instancje Show, Eq, Functor
-- dla 
data Tree a = EmptyTree |  Leaf a | Node a (Tree a) (Tree a)

exampleTree :: Tree Int
exampleTree = Node 1 (Leaf 3) (Node 5 (Leaf 10) EmptyTree)





--
-- Folds
--
-- Ćwiczenia:


-- approxE :: Int -> Double





instance Foldable Tree where
  foldr f seed EmptyTree           = seed
  foldr f seed (Leaf x)            = f x seed
  foldr f seed (Node x left right) = foldr f (f x (foldr f seed right)) left







{-
instance Semigroup Int where
  x <> y = x+y
  
instance Monoid Int where
  mempty = 0
  mappend = (<>)

-}
