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

showCostValue' :: [Double -> Double] -> Double -> Cost Double
showCostValue' list r = (compose list2) r 
  where 
    compose:: [Double -> Cost Double] -> ( Double -> Cost Double )
    compose list = foldr (>=>) return list

    list2 = map addCost list


showCostValue list r = runState (showCostValue' list r) 0 -- Int -> (Double, Int)



----------------------------------------------------------
-- Zadanie:
-- State monad: Praca z generatorami liczb pseudolosowych.
--

type Random a = State Integer a

seed :: Integer
seed  = 123125


randomize :: Random ()
randomize = modify f
  where f x = x^2 - x^3



generatePseudoRandomList :: Integer -> Random [Integer]
generatePseudoRandomList 0 = return []
generatePseudoRandomList n = do
  randomize
  rand <- get
  randtail <- generatePseudoRandomList $ n-1
  return $ rand:randtail

-----------------------------------------------------------
-- Zadanie:
-- State monad: Praca ze stosami. 


type IntStack = State [Int]

emptyStack :: IntStack ()
emptyStack = return ()

push :: Int -> IntStack ()
push _ = return ()

pop :: IntStack Int
pop = return 0

-- Zdefiniujmy następującego:
-- Uwaga. Do tej pory nie pojawił się operator >>.
-- Brzmi ona tak:
-- (>>) :: (Monad m) => m a -> m b -> m b
-- xs >> ys = xs >>= const ys
x = runState ( emptyStack >> push 10 >> push 11 >> pop >> push 20 ) []


---------------------------------------------------------
-- Przyszedł czas na monadę IO!
---------------------------------------------------------


-- Pierwszy przykład z użycia puStrLn, getLine i getChar:

main = do 
  putStrLn "Podaj swoje imię: "
  name <- getLine
  putStrLn $ "Witaj, " ++ name ++ "!"
  putStrLn "Czy powtarzamy jeszcze raz?"
  yesNo <- getChar
  if yesNo == 'y' then main else return ()


-- Zadanie: 
-- Przepisać powyższą definicję main bez notacji do. 
--



-- Zadanie: 
-- Napisać program, który zczyta z linii poleceń nazwę pliku do otwarcia oraz nazwę nowego pliku,
-- który stworzy. Następnie z otwartego pliku wczyta treść odwróci jej kolejność
-- i wynik zapisze w nowym pliku.
main2 :: IO ()
main2 = return ()




