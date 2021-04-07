# Programowanie funkcyjne

## Tomasz Brengos

Wykład 6


## Kod wykładu 
Basics/Lecture05.hs -- kończymy pisanie State1 s a
Basics/Lecture06.hs

---

# State1 s a: jeszcze kilka słów

## Przypomnienie z TK
Mamy następującą jednoznaczną odpowiedniość
```haskell
x -> State s y  ===  x-> s -> (y,s) === (x,s) -> (y,s)
```

# Składanie w Kleisli dla State s y
```haskell
f :: x -> State s y >=> g :: y -> State s z   
                   === 
g" :: (y,s)-> (z,s) . f" :: (x,s) -> (y,s)
```

# Operacja bind i jej interpretacja
```haskell
st :: State s x >>= f :: x -> State s y
                ===
f" :: (x,s) -> (y,s)  .  st :: s -> (x,s)  
```

---


# Dodatkowa funkcjonalność monady State

```haskell
get :: State s s            -- \state   -> (state, state)
put :: s -> State s ()      -- \st1 st2 -> ((), st1) 
```
Dokończyć w pliku Lecture05!

---

# Zadanie 1

```haskell
type Cost = State Int
```

Zadanie: Jak interpretować następujące typy?
```haskell
Double -> Cost Double
Cost String
```

## Zadanie
Rozważmy listę [f1,...,fn] funkcji fi :: Double -> Double. Załóżmy, że 
koszt policzenia wartości dowolnej funkcji powyższego typu dla ujemnych argumentów 
równy jest -3, dla nieujemnych 2. Wielokrotne liczenie wartości funkcji powoduje zsumowanie 
kosztów. Napisać funkcje:
```haskell
addCost :: (Double -> Double) -> Double -> Cost Double
showCostValue :: [Double -> Double] -> Double -> (Int, Double)
```

---

# Zadanie 2

Praca z generatorami liczb pseudolosowych.

## Zadanie
Używając monady State Integer napisać funkcję generatePseudoRandomList
przyjmującą jako argument długość listy (typu Integer) generującą listę pseudolosowych
elementów typu Integer o zadanej długości.

Podpowiedź:
```haskell
type Random = State Integer

randomize :: Random ()

generatePseudoRandomList :: Integer -> Random [Integer]
```

---

# Zadanie 3

Praca ze stosami - wstęp

## Zadanie
Napisać prosty interfejs to obsługi stanu będącego stosem liczb naturalnych dopisując definicje
emptyStack, push i pop dla funkcji, których typy zadane są poniżej.

```haskell
type Stack = State [Integer]

emptyStack :: Stack ()

push :: Integer -> Stack ()

pop  :: Stack Integer
```

---

# Monada IO
Trochę nieformalny sposób myślenia o monadzie IO:
```haskell
IO a    ~    State w a     ~    w -> (a,w)
w = typ obiektu reprezentującego dane dotyczące świata zewnętrznego (stanu dysku, klawiatury monitora, myszki ...)
```

---

# Interfejs monady IO

```haskell
getLine  :: IO String
getChar  :: IO Char
putStrLn :: String -> IO ()
```
Jak składać takie funkcje?

```haskell
main = do
  putStrLn "Podaj swoje imię: "
  name <- getLine
  putStrLn "Witaj, " ++ name ++ "!"
  putStrLn "Czy powtarzamy jeszcze raz?"
  yesNo <- getChar
  if yesNo == 'y' then main
  else return ()
```

## Zadanie
Przepisać definicję main bez użycia do notation.

---

# Interfejs monady IO: obsługa plików, podstawy
```haskell
readFile  :: FilePath -> IO String
writeFile :: FilePath -> String -> IO ()
```

##Zadanie

Napisać program, który zczyta z linii poleceń nazwę pliku do otwarcia oraz nazwę nowego pliku,
który stworzy. Następnie z otwartego pliku wczyta treść odwróci jej kolejność
i wynik zapisze w nowym pliku.
