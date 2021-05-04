# Programowanie funkcyjne

## Tomasz Brengos

Wykład 9


## Kod wykładu
CellularAutomata/


## Literatura

ANSI-Terminal:

https://github.com/UnkindPartition/ansi-terminal/

Comonads and cellular automata:

https://bartoszmilewski.com/2017/01/02/comonads/
https://kukuruku.co/post/cellular-automata-using-comonads/

---

# Stack i nowy projekt

```
> stack new CellularAutomata
```
i zmieniamy w pliku package.yaml
```
dependencies:
...
- ansi-terminal
- colour
- comonad
```

## Zaczynamy!

---

# Komonady:

Komonadą nazywamy trójkę (w, duplicate, extract), gdzie w jest funktorem oraz:
```haskell
extract   :: w a -> a
duplicate :: w a -> w (w a)
```
które spełniają równości dualne do tych spełnianych przez monadę.

Tak samo jak i w przypadku monady, zamiast używać duplicate można stosować:
```haskell
extend :: (w a -> b) -> w a -> w b
```

---

# Stream comonad

```haskell
data Stream a = Stream a (Stream a)
```
Jak zdefiniować dla Stream a funkcje extract i duplicate?


---

# Tape comonad

```haskell
data Tape a = Tape (Stream a) a (Stream a)
```
Tape też ma naturalną strukturę komonady!
