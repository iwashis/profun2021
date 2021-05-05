# Programowanie funkcyjne

## Tomasz Brengos

Wykład 10 


## Kod wykładu
Basics/Lecture10.hs
CellularAutomata/ -- kontynuujemy projekt sprzed tygodnia pisząc testy!


## Literatura

https://hackage.haskell.org/package/QuickCheck
https://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html

---

# Zadanie
Sprawdzić, czy napisane instancje monady dla Stream i Tape spełniają komonadyczne
prawa:
```haskell
extend extract      = id
extract . extend f  = f
extend f . extend g = extend (f . extend g)
```

Napiszemy testy!
