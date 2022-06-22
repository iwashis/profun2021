{-# LANGUAGE  DeriveFunctor #-}
module Lecture03 where -- nazwa modułu, który piszemy


import Data.Foldable
import Data.Monoid
data List a = Empty | Cons a (List a)

x = Cons 1 (Cons 2 Empty)

-- instacja Show dla (List a) (pod warunkiem istnienia
-- zdefiniowanej instancji Show dla typu a).
instance Show a => Show (List a) where
  show Empty       = ""
  show (Cons x xs) = show x ++ maybeComma ++ show xs
    where maybeComma = case xs of
                         Empty -> ""
                         _     -> ","
-- instacja Eq dla List a pod warunkiem zdefiniowanej instancji
-- Eq dla a.
instance (Eq a) => Eq (List a) where
-- (==) :: List a -> List  a -> Bool
  Empty == Empty             = True
  (Cons x xs) == (Cons y ys) = (x == y) && (xs == ys)
  _ == _                     = False

-- instacja Functor dla typu patametrycznego List
instance Functor List where
  -- Warto przypomnieć sobie typ t funkcji, którą definiujemy.
  -- fmap :: (a -> b) -> (List a) -> (List b)
  fmap f Empty       = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

--instancja Semigroup dla List a.
instance Semigroup (List a) where
  -- Operacja półgrupy <>. Zakłada się, że definiujemy operację łączną!
  -- (<>) :: List a -> List a -> List a
  Empty <> x       = x
  (Cons x xs) <> y = Cons x (xs <> y)

-- instancja Monoid dla List a. Poniższa definicja wymaga zdefiniowania instancji
-- Semigroup dla List a. Operacja monoidu (mappend) definiowana jest jako
-- działanie <> półgrupy. Do pełnej definicji instancji monoidu brakuje zatem
-- elementu neutralnego.
instance Monoid (List a) where
 -- mempty :: List a
 -- Zakładamy, że jest to element neutralny działania mappend = (<>).
  mempty = Empty


-- Cwiczenie:  napisac instancje Show, Eq, Functor
-- dla
data Tree a = EmptyTree |  Leaf a | Node a (Tree a) (Tree a)
  deriving (Show, Eq, Functor)

-- Przypominajka dla leniwych (nie na test!). Wiele z instancji można
-- "wywnioskować" operacją deriving.
-- W naszym przypadku będzie to:
--   deriving (Show, Eq, Functor)
-- Powyższa operacja nie zadziała bez dodatkowego Language Pragma na początku
-- pliku. Dzięki
-- {#- LANGUAGE DeriveFunctor -#} na początku pliku wnioskowanie instancji
-- Functor dla parametrycznego typu danych Tree się powiedzie!


exampleTree2 :: Tree String
exampleTree2 =
  Node "a" (Node "b" EmptyTree (Leaf "c")) (Node "d" EmptyTree EmptyTree)




--
-- Folds
-- ----------------
-- Ćwiczenia:
-- ----------------
-- Ćwiczenie 1:
-- init    :: [a] -> [[a]]
-- Ćwiczenie 2:
-- approxE :: Int -> Double
-- Ćwiczenie 3:
-- Napisać foldl za pomocą foldr.
-- Ćwiczenie 4:
-- Zdefiniować foldMap dla Tree:
foldMap1 :: (Monoid m) => (a -> m) -> Tree a -> m
foldMap1 f EmptyTree = mempty
foldMap1 f (Leaf x)  = f x
foldMap1 f (Node x left right) = foldMap1 f left <> f x <> foldMap1 f right

-- Przykład zdefiniowanej instancji Foldable dla Tree.
-- Moglibysmy równie dobrze zastąpić definicję foldr definicją
-- foldMap, którą napisaliśmy powyżej!
instance Foldable Tree where
  foldr f seed EmptyTree           = seed
  foldr f seed (Leaf x)            = f x seed
  foldr f seed (Node x left right) = foldr f (f x (foldr f seed right)) left
