{-# LANGUAGE DeriveFunctor #-}
module Lecture05 where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
-- 
-- Ćwiczenie (do domu):
-- Poznaliśmy już monadę Maybe. Łatwym uogólnieniem tej monady jest
-- monada (Either a) przy ustalonym typie a. 
-- Napisać instancję monady dla (Either a).
--


-- Temat:
-- Kilka słów o Monadzie [-]

-- Rozważmy List:  
-- typ parametryczny izomorficzny z [-]
data List a = Empty | Cons a (List a) 
  deriving (Show, Eq, Functor) -- proszę zwrócić uwagę, że automatycznie wyprowadzamy instancję
                               -- Functor dla typu List.
-- Ćwiczenie: 
-- Napisać instancję monady dla List.
-- Pamiętajmy, że należy też przy okazji wprowadzić 
-- definicję instancji Applicative. Na tym etapie 
-- będziemy się posiłkować plikiem Lecture04.hs i definicją 
-- instancji Applicative dla monady Maybe1. 


instance Monad List where
  -- return :: a -> List a
  -- >>= :: List a -> (a -> List b) -> List b
  return x    = Cons x Empty
  Empty >>= f       = Empty
  (Cons x xs) >>= f = conc (f x) (xs >>= f)
    where conc Empty ts       = ts
          conc (Cons z zs) ts = Cons z (conc zs ts)



instance Applicative List where
  pure    = return
  x <*> y = ap x y







-- Teraz już będziemy pracować z monadą [-] zamiast z List.
--
-- Notacja do i >>= dla monady [-].
-- Przykład
names    = ["Helena", "Lena", "Nela"]
surnames = ["Kowal", "Smith", "Schmidt"]

namesAndSurnames = 
  do
    name    <- names
    surname <- surnames
    return (name, surname)


namesAndSurnames' = names >>=(\name -> ( surnames >>= \surname -> [ name ++ " " ++ surname ] ))




-- Przykład:
-- Ciąg Fibonacciego zapisany w "do notation" 
--
fib :: [Int] 
fib = 0 : 1 : do
          (x,y) <- zip fib $ tail fib
          return (x+y)



-- Temat:
-- Monada State s
--
-- Zacznijmy od napisania instancji dla typu izomorficznego.
newtype State1 s a = State1 { runState1 :: s -> (a,s) } -- proszę zwrócić
-- uwagę na komunikat HLS z sugestią zamiany data na newtype.
--
-- Różnica między data i newtype! 
--
-- 
-- Jeśli st :: State1 s a, to, ponieważ runState1 :: State1 s a -> s -> (a,s),
-- mamy: runState1 st :: s -> (a,s).
--

--
-- Dodefiniujmy jedną pomocnicza funkcja:
--
state1 :: ( s -> (a,s) ) -> State1 s a
state1 = State1


-- Zauważmy, że zachodzi oczywisty związek między funkcjami runState i state:
--
-- runState1 . state1 = id
-- state1 . runState1 = id
--
--

instance Functor (State1 s) where
  -- fmap :: (a->b) -> State1 s a -> State1 s b
  -- st :: State1 s a === s -> (a,s)
  -- State1 s b === s -> (b,s)
  -- f :: a -> b 
  fmap f st = state1 $ ( \(x, s) -> (f x, s) ) . runState1 st


instance Monad (State1 s) where
  -- return :: a -> State1 s a === s -> (a,s)
  return x = state1 $ \st -> (x,st) 
  -- >>= State1 s a -> (a -> State1 s b) -> State1 s b
  -- st >>= f ?
  -- st :: State1 s a === s -> (a,s)
  -- f :: a -> State1 s b === a -> (s -> (b,s)) === (a,s) -> (b,s)
  st >>= f = state1 $ (\(x,s) -> runState1 (f x) s ) . runState1 st



instance Applicative (State1 s) where
  pure    = return
  x <*> y = ap x y




-- Dodatkowo, bardzo często używać będziemy 
put1 :: s -> State1 s ()    -- put1 :: s -> (s-> ((),s))
put1 st = state1 $ \_ -> ((),st)
--
-- get1 :: State1 s s -- get1 :: s -> (s,s)
get1 = state1 $ \state -> (state, state)

-- UWAGA! Pamiętajmy, że powyższe instancje i funkcje pomocnicze zdefiniowane są dla 
-- typu izomorficznego z tym, z którym będziemy pracować. 
-- Od tej chwili będziemy pracowali ze State s a, nie State1 s a :). Co więcej mamy wybór
-- między wersją strict i lazy:
-- Control.Monad.State.Strict
-- oraz
-- Control.Monad.State.Lazy


