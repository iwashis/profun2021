{-# LANGUAGE  TypeSynonymInstances, FlexibleInstances #-}
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


type RuleFun a = Tape a -> a
-- Definicja zasady 30.
rule30 :: Rule
rule30 = Rule "Rule 30" f
  where f (Tape (Stream x _) y (Stream z _)) = x `xor` (y || z)


data Rule = Rule { name         :: String
                 , ruleFunction :: RuleFun Bool
                 }
instance Eq Rule where
  rule1 == rule2 = name rule1 == name rule2

instance Show (RuleFun Bool) where
  show rule = (\x -> if x then '1' else '0') . rule  <$>
    [Tape (Stream.repeat [x]) y (Stream.repeat [z]) | x <- [False,True], y <- [False,True], z <- [False,True]]

-- zasada 90 związana z trójkątem Sierpińskiego
rule90 :: Rule
rule90 = Rule "Rule 90" f
  where
    f (Tape (Stream x _) _ (Stream z _)) = x `xor` z

data Automaton = Automaton { initial :: Tape Bool
                           , rule    :: Rule
                           }

newtype Evolution = Evolution { evolution :: [Tape Bool]
                              }

nullEvolution = Evolution []

calculateEvolution :: Automaton -> Evolution
calculateEvolution automaton = Evolution $ applyRuleFun i r
  where
    i = initial automaton
    r = rule automaton

    applyRuleFun :: Tape Bool -> Rule -> [Tape Bool]
    applyRuleFun initial rule =
      initial : ( (ruleFunction rule `extend`) <$> applyRuleFun initial rule )


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
