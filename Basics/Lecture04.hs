{-# LANGUAGE  DeriveFunctor #-}
module Lecture04 where -- nazwa modułu, który tworzymy 

import Control.Applicative
import Control.Monad

import Data.Functor

-- definiujemy nowy, parametryczny typ danych izomorficzny z Maybe:
data Maybe1 a = Just1 a | Nothing1
  deriving (Eq, Show, Functor) -- wnioskujemy Eq, Show, Functor

--instancja monady dla Maybe1 
instance Monad Maybe1 where
  -- (>>=) :: m a -> (a -> m b) -> m b
  Nothing1  >>= _ = Nothing1    
  (Just1 x) >>= f = f x

  -- return :: a -> m a
  return  = Just1
--musimy też napisać instancję Applicative 
instance Applicative Maybe1 where
  pure    = return
  x <*> y = ap x y 

-- Dla zabawy w komentarzach napiszmy jeszcze
-- jak wygląda definicja składania w kategorii Kleisli dla monady Maybe,
-- czyli jak wygląda definicja operatora >=>. 
-- Weźmy: 
-- f :: a -> Maybe b
-- g :: b -> Maybe c
-- 
-- Operator >=>:
-- f >=> g :: a -> Maybe c
-- (f >=> g) x = case f x of
--                Nothing -> Nothing
--                Just y  -> g y
-- Własności składania w Klesli:
-- 1. (f >=> g) >=> h      ==    f >=> (g >=> h)
-- return >=> g    ==     g >=> return    ==    g

--Ćwiczenie. Wróćmy do Maybe i pobawmy się częściowymi funkcjami
--head' :: [a]-> Maybe a i tail' :: [a] -> Maybe [a]
head' :: [a] -> Maybe a
head' []     = Nothing
head' (x:xs) = Just x

tail' :: [a] -> Maybe [a]
tail' []     = Nothing
tail' (x:xs) = Just xs 

-- Ćwiczenie:
-- Napisać funkcję takeThird :: [a] -> Maybe a, która zwraca trzeci element z kolei
-- w liście używając head', tail' oraz >>=.
takeThird xs = tail' xs 
               >>= tail' 
               >>= head'
