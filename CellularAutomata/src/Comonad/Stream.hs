{-# LANGUAGE DeriveFunctor #-}
module Comonad.Stream (
  Stream (..)
  , take
  )
  where

import Prelude hiding (tail, sum, take)
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
  duplicate stream        = Stream stream (duplicate $ tail stream)
