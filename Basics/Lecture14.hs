{-# LANGUAGE  GADTs #-}
module Lecture14 where

import Data.List



--Przypomnijmy sobie definicje parametrycznego typu danych Maybe:
data Maybe' a = Nothing' | Just' a

-- Definicję Maybe możemy przepisać używając notacji GADT w następujący sposób:
data Maybe'' a where
  Nothing :: Maybe'' a
  Just    :: a -> Maybe'' a

-- To samo mozemy zrobic z listami:
data List a = Empty | NonEmpty a (List a)
-- ktore w notacji GADT zdefiniowalibysmy tak:
data List' a where
  Empty'    :: List' b
  NonEmpty' :: b -> List' b -> List' b



-- Przykład 1
-- Rozważmy typ reprezentujący proste wyrażenia arytmetyczne:
data ArExpr = Number Int | Addition ArExpr ArExpr

-- Widać, że wyrażenie 1+(2+3) możemy reprezentować jako
-- następujący element typu ArExpr
-- Addition (Number 1) (Addition (Number 2) (Number 3))

-- Zwróćmy też uwagę, że wyrażenie
-- 1+(2+) jest błędne i nie ma swojego odpowiednika jako element typu ArExpr.
-- O to nam chodziło!

-- Dla elementów typu AArExpr mozemy zdefiniować funkcję oblicząjącą wartość
-- tego wyrażenia w następujący łatwy sposób:
eval :: ArExpr -> Int
eval (Number n) = n
eval (Addition x y) = eval x + eval y



-- A teraz chcemy rozważyć trudniejszy przykład wyrażeń. Co byłoby gdybyśmy
-- chcieli rozbudować język wyrażeń arytmetycznych jak wyżej wprowadzając wyrażenia
-- logiczne (np sprawdzenie równości między dwoma elementami) i instrukcję if x then y else z?
-- Innymi słowy mamy wyrażenia, zdefiniowane następująco:
-- Dowolna liczba naturalna jest wyrażeniem
-- True i False są wyrażeniami
-- Jesli x i y są liczbami naturalnymi to (x+y) jest wyrażeniem
-- jesli x i y są oba wyrażeniami na liczbach naturalnych lub oba są wyrażeniami logicznymi
-- to (x == y) wyrażeniem. I w końcu jeśli x jest wyrażeniem logicznym, y i z są dowolnymi wyrażeniami
-- tego samego typu to if x then y else z jest wyrażeniem.

-- Przykład:
-- if (2+3 = 4) then 7 else 8 --dobre wyrazenie
-- if 2 then 5 else False -- zle
{-
-- Najprostszym typem danych (a jednocześnie błędnym) jaki się nasuwa od razu
-- jako typ reprezentujący powyższe wyrażenia, jest:

data SimpleExpr = I Int | B Bool | Add SimpleExpr SimpleExpr | Eq SimpleExpr SimpleExpr
                  | If SimpleExpr SimpleExpr SimpleExpr

 -- Niestety, jak widać na przykładzie poniżej, jesteśmy łatwo w stanie wymyślić przykład
 -- elementu typu SimpleExpr, który nie będzie preprezentować żadnego sensownego wyrażenia.
 --

ugly :: SimpleExpr
ugly = If (I 4) (B True) (I 5) -- odpowiednik wyrażenia if 4 then True else 5

-- Nawet jeśli uznamy, że nie jest to dla nas problemem, to pojawia się tez następujące pytanie:
-- Jak zdefiniować funkcję liczącą wartość wyrażeń typu SimpleExpr?
-- Niektore z nich nie mają sensu, niektóre są wartościami typu Int, a niektóre Bool.
-- Może więc:
-- eval :: SimpleExpr -> Maybe (Either Int Bool) ???
-- Wygląda to fatalnie!
-}

-- Z pomocą przychodzą GADTy:
data Expr a where
  I   :: Int  -> Expr Int
  B   :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Eq  :: (Eq b) => Expr b -> Expr b -> Expr Bool
  If  :: Expr Bool -> Expr b -> Expr b -> Expr b

-- w takim przypadku, funkcja licząca wartość wyrażenia wygląda następująco:
run :: Expr a -> a
run (I n) = n
run (B b) = b
run (Add x y) = run x + run y
run (Eq x y)  = run x == run y
run (If b x y) = if run b then run x else run y


good = If (Eq (I 4) (Add (I 2) (I 3))) (B True) (B False)



-- Przykład 2.
-- Rozważmy Set-funktor Powerset :: Set -> Set. Czy jestesmy w latwy sposob
-- zdefiniowac analog Powerset w Haskellu?
--
-- Powerset X = zbior wsyzstkich podzbiorow X
-- i na strzalkach

data Powerset a where
  FromList :: (Ord a) => [a] -> Powerset a -- najwazniejszymi cechami zbioru elementów typu a (w porownaniu z cechami list [a])
  -- są: 1. elementy nie mogą sie powtarzać 2. kolejnosc nie ma znaczenia, czyli:
  -- {x,x,y} == {x,y} oraz {x,y} = {y,x}. Osiągniemy to dodając wymóg na to aby a miało zdefiniowaną instancję Ord.
  --
  --
  -- W tym miejscu nalezy zwrocic uwagę na fakt, iż nie jestesmy uzywając takiej definicji Powerset a dostać elementy Powerset a dla dowolnego
  -- typu a. Jesli definicja jest taka jak powyzsza, a musi miec zdefiniowana instancje Ord.
  --
  --

-- teraz mozemy zdefiniowac również równość między elementami typu Powerset a (nie potrzeba explicite wymagać od a instancji Ord).
instance Eq (Powerset a) where
  (FromList list1) == (FromList list2) = shrink list1 == shrink list2

instance Show a => Show (Powerset a) where
  show (FromList list) = map replace $ show $ shrink list
    where
      replace '[' = '{'
      replace ']' = '}'
      replace  x  = x

-- Rozwazmy nastepujacy przykladowy zbiór:
exampleSet1 = FromList [1,1,1,2,5,1]
exampleSet2 = FromList [5,2,2,1]

evaluate :: Powerset a -> [a]
evaluate (FromList list) = shrink list


shrink :: (Ord a) => [a] -> [a]
shrink = sort . removeDuplicates
  where
    removeDuplicates [] = []
    removeDuplicates (x:xs) = x : filter (/=x) (removeDuplicates xs)




-- Przykład 3.
-- typ reprezentujący wszystkie typy dla ktorych instnieje zdefiniowana
-- instancja Show.
data Showable where
  Element :: (Show a) => a -> Showable

instance Show Showable where
  show (Element x) = show x

list :: [Showable]
list = [Element 1, Element "Tomek", Element 1.9]
