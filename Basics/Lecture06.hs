module Lecture06 where

import Control.Monad.State.Strict


--------------------------------------------------------
-- Zadanie: 
--
-- State monad: Mamy dwie funkcje
-- f :: Double -> Double
-- g :: Double -> Double
--
-- Wiemy, że koszt policzenia wartości dowolnej 
-- funkcji dla ujemnych argumentów jest rowny -3, dla nieujemnych 2. 
-- Przy wielokrotnym składaniu funkcji koszty obliczenia wartości się sumują.
-- Jaka jest funkcja kosztu złożenia g . f ? 
--

-- f |-> f' :: (Double, Int) -> (Double, Int)
-- Wiedząc, że nasza kategoria Hask jest CCC możemy to zapisać jako:
-- f |-> f' :: Double -> State Int Double
-- Dla uproszczenia zapisu wprowadźmy jeszcze definicję:
type Cost = State Int
-- Wtedy mamy:
-- f' :: Double -> Cost Double
-- Jak zdefiniować f' (nazywaną poniżej addCost f)?

addCost :: (Double -> Double) -> Double -> Cost Double
addCost f r = do
 modify (\st -> if r < 0 then st - 3 else st+2)
 return $ f r



-- Jak pracować z takimi funkcjami Double -> Cost Double?
--


showCostValue list r = runState (composeCost list r) 0 -- Int -> (Double, Int)
  where
    composeCost :: [Double -> Double] -> Double -> Cost Double
    composeCost list r =  foldr (>=>) return  (map addCost list) r
    
  
    


----------------------------------------------------------
-- Zadanie:
-- State monad: Praca z generatorami liczb pseudolosowych.
--

type Random a = State Integer a

seed :: Integer
seed  = 123125


randomPickInteger :: Random Integer
randomPickInteger = do
  x <- get
  put $ shuffle x
  get
    where shuffle x = x^2-x^3


randomPickList :: Integer -> Random [Integer]
randomPickList 0 = return []
randomPickList n = do 
  x  <- randomPickInteger
  xs <- randomPickList $ n-1
  return (x:xs)
