{-# LANGUAGE  FlexibleInstances #-}
module Automaton ( Automaton (..)
                 , Evolution (..)
                 , allAutomata
                 , calculateEvolution
                 , showEvolution
                 , nullEvolution
                 , Rule (..)
                 , rule30
                 , automaton30
                 , automaton90
                 ) where

import Comonad.Tape
import Comonad.Stream as Stream
import Control.Comonad

xor x y = (not x || not y) && ( x || y )

-- zdefiniujmy alias typu, tak, żeby wiedzieć czym dokładnie
-- i w jakim celu stosowane są funkcje typu Tape Bool -> Bool
type Rule = Tape Bool -> Bool

-- Definicja zasady 30.
rule30 :: Rule
rule30 (Tape (Stream x _) y (Stream z _)) = x `xor` (y || z)

-- zasada 90 związana z trójkątem Sierpińskiego
rule90 :: Rule
rule90 (Tape (Stream x _) _ (Stream z _)) = x `xor` z


instance Show Rule where
  show rule = (\x -> if x then '1' else '0') . rule  <$>
    [Tape (Stream.repeat [x]) y (Stream.repeat [z]) | x <- [False,True], y <- [False,True], z <- [False,True]]


-- typ danych Automaton:
data Automaton = Automaton { name    :: String
                           , initial :: Tape Bool
                           , rule    :: Rule
                           }

-- przyda nam się instancja Eq dla Automaton
instance Eq Automaton where
  aut1 == aut2 =
    name aut1 == name aut2

automaton30 = Automaton "Rule 30" initialTape rule30
automaton90 = Automaton "Rule 90" initialTape rule90

allAutomata :: [Automaton]
allAutomata = [ automaton30
              , automaton90
              ]

-- alias typu Evolution. Evolution to nic innego jak lista elementów
-- typu Tape Bool.
type Evolution = [Tape Bool]

nullEvolution :: Evolution
nullEvolution = []

calculateEvolution :: Automaton -> Evolution
calculateEvolution automaton = applyRuleFunction i r
  where
    i = initial automaton
    r = rule automaton
    applyRuleFunction :: Tape Bool -> Rule -> Evolution
    applyRuleFunction initial rule =
      initial : ((rule `extend`) <$> applyRuleFunction initial rule )


showEvolution :: Evolution -> (Bool -> Char) -> Int -> Int -> [String]
showEvolution evol f width height = Prelude.take height $
  show . frame width' <$> (fmap f <$> evol)
  where
    width' = (width `div` 2) - 4
