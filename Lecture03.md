# Programowanie funkcyjne

## Tomasz Brengos

Wykład 3


## Kod wykładu 
Basics/Lecture03.hs


---

# Typeclasses

W REPL:
```haskell
ghci> :t (==)

ghci> :t show

ghci> :t (<)

ghci> :t read

ghci> :t fmap
```

---

# Definiowanie własnych instancji

```haskell

data List a = Empty | Head a (List a)

instance Show a => Show List a where
  ...

instance Eq a => Eq (List a) where
  ...

instance Functor List where
  ...

```

---

# Definiowanie własnych typeclasses

## Przyklady typeclass predefiniowanych w Haskellu (wersja skrocona):

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b 

class Show a where
  show :: a -> String

class Semigroup a where
  (<>) :: a -> a -> a

class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mappend = (<>)
```

---


# Rozszerzmy nasze definicje instancji dla List

```haskell
instance Semigroup (List a) where
...

instance Monoid (List a) where
...
```

---


# Funktory

Ćwiczenia:
```haskell
data Maybe1 a = Just a | Nothing -- Maybe a := a + {*}

instance Functor Maybe1 where
  fmap = ...
```

```haskell
data Either1 a b = Left a | Right b

instance Functor (Either1 a) where
  fmap = ...
```

---

# Folds (klasycznie)

Niech:
```haskell
(*)  :: a -> b -> b
(#)  :: b -> a -> b
seed :: b
ai   :: a
```
Rozważmy następujące przyporządkowania:
```haskell
* seed [a1,...an] -> a1*(a2*...(an*seed))...)
# seed [a1,...an] -> ((..(seed#a1)#a2#...)#an
```


```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f seed []     =  seed
foldl f seed (x:xs) =  foldl f (f seed x) xs
```

```haskell 
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f seed []     = seed
foldr f seed (x:xs) = f x (foldr seed xs)
```

Obie funkcje działają leniwie! Jedną z nich można przepisać do 
wersji gorliwej:
```haskell
foldl' f seed []     = seed
foldl' f seed (x:xs) = let z = f seed x in seq z (foldl' f z xs)
```

---

# Folds (klasycznie)

## Ćwiczenia

1) Używając funkcji foldr zdefiniowac funkcję 
```haskell
init :: [a] -> [[a]] 
```
zwracającą wszystkie
prefiksy argumentu, np. 
```haskell
init "tomek" = [[],"t","to","tom","tome","tomek"]
```
2) Używając foldl zdefiniować 
```haskell 
approxE :: Int -> Double
```
która dla argumentu n zwraca
przybliżoną wartość liczby Eulera 
(używajac klasycznego wzoru na sumę 
odwrotności silnii kolejnych liczb nautralnych)

## Ćwiczenia ciekawsze

3) Zapisać foldl używajac funkcji foldr. 

---

# Definicja foldl za pomocą foldr: 

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f v []       = v
foldl f v (x : xs) = foldl f (f v x) xs


foldl f v xs = g xs v
    where
        g []     v = v
        g (x:xs) v = g xs (f v x)


foldl f v xs = g xs v -- (g xs) v
    where
        g []     = \v -> v -- to samo co id
        g (x:xs) = \v -> g xs (f v x)


foldl f seed xs = (foldr step id xs) seed
  where 
    step x g a = g (f a x)
```

---

# Definicja foldl za pomocą foldr:

```haskell 
myFoldl f seed xs = (foldr step id xs) seed
  where 
    step x g a = g (f a x)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f seed []     = seed
foldr f seed (x:xs) = f x (foldr seed xs)
```

Sprawdźmy obliczenia na przykładzie:

```haskell
myFoldl (/) 1 [2,3] = (foldr step id [2,3]) 1 ->
(step 2 (foldr step id [3])) 1 ->
(step 2 (step 3 (foldr step id [])) 1 -> 
(step 2 (step 3 id)) 1 ->
(step 2 (\n -> id ((/) n 3))) 1 ->
(step 2 (\n -> (/) n 3)) 1 ->
(\m -> (\n-> (/) n 3) ((/) m 2)) 1 ->
(\n -> (/) n 3) ((/) 1 2) ->
(\n -> (/) n 3) 1/2 ->
(1/2)/3 -> 0.1666666....
```

---

# Foldables bardziej ogólnie

```haskell
data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
```
Spróbujmy napisać wersje foldr dla Tree a zamiast [a].

```haskell
foldr :: (a -> b -> b) -> b -> Tree a -> b
foldr f seed Empty               = seed
foldr f seed (Leaf x)            = f x seed
foldr f seed (Node x left right) = foldr f (f x (foldr f seed right)) left
```

---

# Foldable typeclass

```haskell
class Foldable t where
  foldr   :: (a -> b -> b) -> b -> t a -> b
  foldl   :: (b -> a -> b) -> b -> t a -> b
  foldMap :: (Monoid m) => (a -> m) -> t a -> m
  fold    :: (Monoid m) => t m -> m
  ...
```
Minimalna definicja instancji zawierać musi definicję
```haskell
foldr | foldMap
```

## Ćwiczenie

Zdefiniować foldMap za pomocą foldr i odwrotnie.

---
