module Automaton ( Automaton (..)
                 , Evolution (..)
                 , allAutomata
                 , calculateEvolution
                 , showEvolution
                 ) where

import Comonad.Tape
import Comonad.Stream
import Control.Comonad

xor x y = (not x || not y) && ( x || y )

type Rule a = Tape a -> a
-- Definicja zasady 30.
rule30 :: Rule Bool
rule30 (Tape (Stream x _) y (Stream z _)) = x `xor` (y || z)


-- zasada 90 związana z trójkątem Sierpińskiego
rule90 :: Rule Bool
rule90 (Tape (Stream x _) _ (Stream z _)) = x `xor` z

data Automaton = Automaton { initial :: Tape Bool
                           , rule :: Rule Bool
                           }

newtype Evolution = Evolution { evolution :: [Tape Bool]
                              }

emptyEvolution = Evolution []

calculateEvolution :: Automaton -> Evolution
calculateEvolution automaton = Evolution $ applyRule i r
  where
    i = initial automaton
    r = rule automaton

    applyRule :: Tape a -> Rule a -> [Tape a]
    applyRule initial rule =
      initial : ( (rule `extend`) <$> applyRule initial rule )


showEvolution :: Evolution -> (Bool -> Char) -> Int -> Int -> [String]
showEvolution evol f width height = Prelude.take height $
  show . frame width' <$> (fmap f <$> evolution evol)
  where
    width' = (width `div` 2) - 4

automaton30 = Automaton initialTape rule30
automaton90 = Automaton initialTape rule90

allAutomata :: [Automaton]
allAutomata = [ automaton30
              , automaton90
              ]
