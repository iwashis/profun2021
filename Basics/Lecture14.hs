{-# LANGUAGE  GADTs #-}
module Lecture14 where


data Maybe' a = Nothing' | Just' a


data Maybe'' a where
  Nothing :: Maybe'' a
  Just    :: a -> Maybe'' a

data List a = Empty | NonEmpty a (List a)

data List' a where
  Empty'    :: List' b
  NonEmpty' :: b -> List' b -> List' b


data ArExpr = Number Int | Addition ArExpr ArExpr

-- 1+(2+3) ----> ArExpr
-- Addition (Number 1) (Addition (Number 2) (Number 3))
--
-- 1+(2+) ---/---> ArExpr

eval :: ArExpr -> Int
eval (Number n) = n
eval (Addition x y) = eval x + eval y


-- +
-- =
-- if then else
-- if (2+3 = 4) then 7 else 8 --dobre wyrazenie
-- if 2 then 5 else False -- zle
  {-
data SimpleExpr = I Int | B Bool | Add SimpleExpr SimpleExpr | Eq SimpleExpr SimpleExpr
                  | If SimpleExpr SimpleExpr SimpleExpr


ugly :: SimpleExpr
ugly = If (I 4) (B True) (I 4)

-}
-- eval :: SimpleExpr -> Maybe (Either Int Bool) ???
--

data Expr a where
  I   :: Int  -> Expr Int
  B   :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Eq  :: (Eq b) => Expr b -> Expr b -> Expr Bool
  If  :: Expr Bool -> Expr b -> Expr b -> Expr b


run :: Expr a -> a
run (I n) = n
run (B b) = b
run (Add x y) = run x + run y
run (Eq x y)  = run x == run y
run (If b x y) = if run b then run x else run y




good = If (Eq (I 4) (Add (I 2) (I 3))) (B True) (B False)


-- Powerset :: Set -> Set
-- Powerset X = zbior wsyzstkich podzbiorow X
-- i na strzalkach

data Powerset a where
  FromList :: (Ord a, Eq a) => [a] -> Powerset a
  Return   :: a -> Powerset a
-- dokonczyc
--
evaluate :: Powerset a -> [a]
evaluate (FromList list) = removeDuplicates list
  where
    removeDuplicates [] = []
    removeDuplicates (x:xs) = x : filter (/=x) (removeDuplicates xs)

evaluate (Return x) = [x]
