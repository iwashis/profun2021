# Programowanie funkcyjne

## Tomasz Brengos

Wykład 14


## Kod wykładu

Basics/Lecture14.hs

---

# GADTs

```haskell
data Maybe a where
  Nothing :: Maybe a
  Just    :: a -> Maybe a

data List a where
  Nil     :: List a
  Cons    :: a -> List a -> List a
```
---

## Literatura
https://wiki.haskell.org/GADTs_for_dummies
https://downloads.haskell.org/~ghc/6.6/docs/html/users_guide/gadt.html
