# Programowanie funkcyjne

## Tomasz Brengos

Wykład 8


## Kod wykładu 
Basics/ArithmeticParser.hs


## Literatura

Monadic Parser Combinators, G. Hutton, E. Meijer (1996) 


---


# Monadyczny parser


```haskell
Parser a = String -> [ (a, String) ]
```

## Zadanie
Napisać parser wyrażeń arytmetycznych zdefiniowany następującą gramatyką
```
Cyfra    = 0 | .. | 9
Liczba   = Cyfra | Cyfra Liczba
Operacja = + | *
WyrAryt  = Liczba | (WyrAryt Operacja WyrAryt)
CiagWyrazen = WyrAryt; | WyrAryt; CiagWyrazen
```

---

# Jak myśleć o parserach?

Niech p1 :: Parser a. Używając notacji diagramów sznurkowych
możemy o nim myśleć następująco:
```haskell
 String |-----|     (a, String)
--------|     |   /
        | p1  |--< ...      
        |     |   \    
        |_____|     (a, String)
```

## Pytanie
Czym jest >> i >>= ?

