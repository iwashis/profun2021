module Philosopher where

import Control.Monad
import System.Random
import Control.Concurrent
import Control.Concurrent.STM


data Utensil = Utensil -- typ jednoelementowy
type Fork = TMVar Utensil -- zmienna mutowalna typu Utensil

newFork :: Utensil -> IO Fork -- "zapełnienie" mutowalnej zmiennej wartością Utensil
newFork = newTMVarIO

pickup :: Fork -> STM Utensil
pickup = takeTMVar

putdown :: Fork -> STM ()
putdown fork = putTMVar fork Utensil


type TableIO = IO [Fork]

data PhState = Hungry | Eating | Thinking

instance Show PhState where
  show Hungry   = " Hungry "
  show Eating   = " Eating "
  show Thinking = "Thinking"

data Philosopher = Philosopher String PhState

instance Eq Philosopher where
  (Philosopher name1 _) == (Philosopher name2 _) = name1 == name2

toHungry :: Philosopher -> Philosopher
toHungry (Philosopher name _) = Philosopher name Hungry

toEating :: Philosopher -> Philosopher
toEating (Philosopher name _) = Philosopher name Eating

toThinking :: Philosopher -> Philosopher
toThinking (Philosopher name _) = Philosopher name Thinking

doing :: Philosopher -> IO ()
doing (Philosopher name what) = do
  -- putStrLn $ name ++ " is " ++ show what ++ "!"
  delay <- randomRIO (2,10) :: IO Int
  threadDelay (delay * 1000000)

pickUpForks :: Philosopher -> (Fork, Fork) -> IO ()
pickUpForks (Philosopher name _) (left, right) = do
  -- putStrLn $ name ++ " is trying to pick up forks!"
  (lUt, rUt) <- atomically $ do
    l <- pickup left
    r <- pickup right
    return (l,r)
  return ()
  -- putStrLn $ name ++ " picked up forks!"


putDownForks :: Philosopher -> (Fork, Fork) -> IO ()
putDownForks _ (left,right)  = atomically $ do
  putdown left
  putdown right
