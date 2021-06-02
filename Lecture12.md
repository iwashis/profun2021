# Programowanie funkcyjne

## Tomasz Brengos

Wykład 12


## Kod wykładu
Basics/Lecture12.hs

DiningPhilosophers/

---

# Współbieżność (concurrecy)
```haskell
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
  forkIO $ replicateM_ 5 $ putStrLn "a"
  replicateM_ 5 $ putStrLn "b"
  getLine
  return ()
```

I inny przykład:

```haskell
alpha = ["a", "b", "c", "d", "e"]

main2 = do
  mapM_ forkIO [ replicateM_ 3 (putStr s) | s <- alpha ]
  getLine
  return ()
```

---

## Literatura
https://hackage.haskell.org/package/stm
https://en.wikipedia.org/wiki/Dining_philosophers_problem
https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch07.html
