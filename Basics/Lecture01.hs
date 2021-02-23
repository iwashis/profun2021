{-# LANGUAGE  BangPatterns #-}
-- Linijka powyżej stanie się jasna pod koniec wykładu 
module Lecture01 where -- nazwa modułu, który piszemy


import Data.List

--przykład funkcji napisanej w języku Haskell:
quicksort :: Ord a => [a] -> [a] -- tą linię można usunąć. Haskell sam wywnioskuje typ funkcji.
quicksort []     = []
quicksort (x:xs) = quicksort le ++ [x] ++ quicksort gr
  where
    le = filter (< x) xs
    gr = filter (>=x) xs


-- przykład deklaratywnej definicji funkcji sumowania wartości numerycznych
-- w liście: 
sumUp :: Num a => [a] -> a
sumUp = foldl' (+) 0

-- 
-- Leniwa ewaluacja
--

f :: Int -> Int -> Int
f x y = if x > 0 then x+1 else x-1

val = f 1 (product [1..])



--propozycja definicji length1
length1 :: [a] -> Int 
length1 []     = 0
length1 (x:xs) = 1+(length1 xs)


--pomocnicza wartość val2
val2 = length1 [1,x]
  where x = product [1..]

--inny length2
length2 :: [Int] -> Int
length2 [] = 0
length2 (x:xs) = if x > 0 then r else r
  where r = 1+length2 xs
 

--funkcja sumująca wartości numeryczne w liście (znowu!)
sum' []     = 0
sum' (x:xs) = x + sum' xs


--używając seq możemy wymusić częściowe wyliczenie wartości w trakcie wykonywania
--obliczania.
sum'' xs = go xs 0
  where go [] accum     = accum
        go (x:xs) accum = let s = x+accum in seq s $ go xs s

--Druga, bardziej przejrzysta metoda radzenia sobie z problemami z pamięcią przy 
--leniwej ewaluacji: Bang Patterns. Proszę zwrócić uwagę na pierwszą linię w naszym
--pliku.
sum''' xs = go xs 0
  where go []     !accum = accum
        go (x:xs) !accum = go xs (accum + x)
      


