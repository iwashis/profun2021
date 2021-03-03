{-# LANGUAGE RecordWildCards #-}
module Lecture02 where -- nazwa modułu, który piszemy

data BasicColours = Red
                    | Green
                    | Blue



someFunction :: BasicColours -> Int
someFunction Red   = 2
someFunction _     = 1


data Shape = Rectangle (Double,Double) (Double,Double) 
           | Circle (Double, Double) Double     
           | Point (Double, Double)
  deriving Show

exampleRect :: Shape
exampleRect = Rectangle (0,1) (2,4)

exampleCirc :: Shape
exampleCirc = Circle (0,0) 5

changeRadiusIfCircle :: Shape -> Shape
changeRadiusIfCircle (Circle point r) = Circle point (r+5)
changeRadiusIfCircle x = x


-- Dokładne definiowanie typów. Można tak: 
data Person1 = Person1 String String String Integer 
  deriving Show
-- ale też można tak:
data Person2 = Person2 { name    :: String
                       , surname :: String
                       , address :: String 
                       , age     :: Integer
                       }
  deriving (Eq, Show)
-- rozważmy trzy przykłady:
examplePerson1 = Person1 "Tomasz" "Kowalski" "Warszawa" 20
examplePerson2 = Person2 "Tomasz" "Kowalski" "Warszawa" 20
examplePerson3 = Person2 { surname = "Kowalski"
                         , name    = "Tomasz"
                         , age     = 20
                         , address = "Warszawa" 
                         }



addMr :: Person2 -> Person2
addMr p@(Person2 name surname address age) = 
  if surname /= "" then Person2 ("Mr. " ++ name) surname address age
                   else p
 
-- przykład operowania na typie Person2 używając RecordWildCards
-- (patrz pierwsza linijka tego pliku)
addMr2 :: Person2 -> Person2
addMr2 p@Person2 { name    = name
                 , surname = surname
                 , .. 
                 } =
   if surname /= "" then Person2 { name = "Mr. " ++ name, .. }
                    else p 

--
-- Rekurencyjne typy danych:
--

data IntList = EmptyList | Head Int IntList
  deriving Show

list = Head 30 (Head 10 EmptyList)

-- napisać definicję funkcji
-- length :: IntList -> Int

length3 :: IntList -> Int
length3 EmptyList   = 0
length3 (Head _ xs) = 1 + length3 xs

--
-- Synonimy typów, czyli type 
--

measure :: Double -> Double -> Double
measure w h = w*h 

type Width  = Double
type Height = Double
type Area   = Double

measure2 :: Width -> Height -> Area
measure2 w h = w*h

--
-- Typy parametryczne
--
data List a = Empty | NonEmpty a (List a)
--
-- napisać funkcję toList :: List a -> [a]
--


--
-- Przykład z dwoma parametrami typów:
--
data Tree a b = EmptyTree | Node a (Tree a b) (Tree b a)
  deriving Show
