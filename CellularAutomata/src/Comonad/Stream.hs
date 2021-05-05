{-# LANGUAGE DeriveFunctor #-}
module Comonad.Stream (
  Stream (..)
  , take
  , repeat
  )
  where

import Prelude hiding (repeat, tail, sum, take)
import qualified Data.List.NonEmpty as NonEmpty
import Control.Comonad

data Stream a = Stream -- pierwszy przykład komonady
                { head  :: a
                , tail  :: Stream a
                }
  deriving Functor


-- funkcja pomocnicza
take :: Int -> Stream a -> [a]
take n (Stream x tail)
  | n <= 0    =  []
  | otherwise =  x : take (n-1) tail

-- zdefiniowanie instancji komonady sprowadza się do zdefiniowania
-- extract :: w a -> a
-- duplicate :: w a -> w w a lub extend :: (w a -> b) -> w a -> w b
-- Warto zajrzeć na https://hackage.haskell.org/package/comonad
instance Comonad Stream where
  extract (Stream x tail) = x
  duplicate stream        = Stream stream $ duplicate $ tail stream


-- stała oznaczająca indeks, dla którego wpisy o indeksie większym
-- niż streamBound nie będą brane pod uwagę w instancjach Eq i Show
-- dla Stream a.
streamBound = 10
-- zdefiniujmy Eq na potrzeby testów.
-- Choć z logicznego punktu widzenia to może nie koniecznie mieć sens.
instance Eq a => Eq (Stream a) where
  stream1 == stream2 =
    take streamBound stream1 == take streamBound stream2

-- to samo Show. QuickCheck wymaga instancji Show
-- dla testowanych danych (żeby mógł wyświetlać kontrprzykłady)
instance Show a => Show (Stream a) where
  show stream = show (take streamBound stream) ++ "..."

-- funkcja, która dla argumentu [a1...an] zwraca Stream
-- a1..an a1..an a1..an...
repeat :: [a] -> Stream a
repeat list = go list list
  where
    go _ []      = error "The value of repeat is undefined for []"
    go [] l2     = go l2 l2
    go (x:xs) l2 = Stream x (go xs l2)


