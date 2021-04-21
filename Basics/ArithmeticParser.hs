{-# LANGUAGE  LambdaCase #-}
-- Parser przedstawiony poniżej bazuje na artykule
-- "Monadic Parser Combinators", G. Hutton, E. Meijer (1996)
--
module ArithmeticParser where

import Control.Monad.State.Strict
import Control.Applicative
-- Gramatyka wyrażeń arytmetycznych:
-- Operacja  = + | *
-- Cyfra     = 0 | 1 | .. | 9
-- Liczba    = Cyfra | Cyfra Liczba
-- WyrAryt = Liczba | (WyrAryt Operacja WyrAryt)

-- Zakładamy, jak w artykułe, że parsery to:
-- Parser a === String -> [(a, String)]

type Parser a = StateT String [] a

-- Uwaga: Parsery monadyczne są dostępne w standardowych
-- bibliotekach Haskella. Ale my napiszemy wszystko od początku.
--
-- String -> Maybe Int
--
--

item :: Parser Char -- String -> [(Char, String)]
item = StateT $ \case 
                    [] -> []
                    (x:xs) -> [(x,xs)]


zero :: Parser a
zero = mzero

sat :: (Char -> Bool) -> Parser Char
sat chi = do
  x <- item
  if chi x then return x else zero

isDigit x = (x <= '9') && ( x >= '0')

digit  = sat isDigit

many1 ys = do
  x  <- ys
  xs <- many ys
  return (x:xs)

digits :: Parser String
digits = many1 digit


integer :: Parser Int
integer = fmap read digits

plus :: Parser (Int -> Int -> Int) 
plus = sat ( == '+' ) >> return (+)

times :: Parser (Int -> Int -> Int)
times = sat (== '*' ) >> return (*)

operation = plus `mplus` times
space = sat (==' ')

simple = do
  x <- integer
  many space
  op <- operation
  many space
  y <- integer
  return (op x y)

leftBracket = sat (=='(')
rightBracket = sat (==')')

between :: Parser a -> Parser b -> Parser c -> Parser c
between left right parser = do
  left
  x <- parser
  right
  return x

lessSimple = between leftBracket rightBracket simple


arExp :: Parser Int
arExp = integer `mplus` between leftBracket rightBracket opExp
  where
    opExp = do
      many space
      x <- arExp
      many space
      op <- operation
      many space
      y <- arExp
      many space
      return (op x y)


arExpWithColon :: Parser Int
arExpWithColon = do
  x <- arExp
  sat(==';')
  return x
