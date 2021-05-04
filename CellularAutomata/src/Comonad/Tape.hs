{-# LANGUAGE  DeriveFunctor #-}
module Comonad.Tape
  ( Tape (..)
  , FramedTape (..)
  , initialTape
  , frame
  )
    where

import Control.Comonad
import Comonad.Stream as Stream -- importujemy nasz kod z pliku src/Comonad/Stream.hs


data Tape a = Tape
              { left  :: Stream a -- taśma po lewej od głowicy
              , value :: a -- wartość wskazywana przez głowicę taśmy
              , right :: Stream a -- taśma po prawej od głowicy
              }
  deriving Functor

instance Comonad Tape where
  extract = value -- extract :: Tape a -> a
  duplicate uni = Tape  -- duplicate :: Tape a -> Tape (Tape a)
                { left  = tailLeft
                , value = uni
                , right = tailRight
                }
    where
      tailLeft  = shiftLeft <$> Stream uni tailLeft
      tailRight = shiftRight <$> Stream uni tailRight
      shiftLeft (Tape (Stream a ta) b c)  = Tape ta a (Stream b c)
      shiftRight (Tape a b (Stream c tc)) = Tape (Stream b a) c tc


initialTape :: Tape Bool -- początkowa taśma z głowicą na True, reszta taśmy False
initialTape = Tape falses True falses
  where falses = Stream False falses

data FramedTape a = FramedTape -- ta struktura danych uzywana będzie do wyświetlania
                    { tape  :: Tape a
                    , width :: Int
                    }
  deriving Functor

-- pokazujemy tylko część taśmy o promieniu reprezentowanym przez wartość tape ft
-- gdzie ft :: FramedType a. Całej taśmy nie da się wyświetlić :(
instance (Show a) => Show (FramedTape a) where
  show (FramedTape tape n) = removeApostrophes . show $
        (reverse . Stream.take n $ left tape)
        ++ [value tape]
        ++ Stream.take n (right tape)
    where
      removeFirst :: Char -> String -> String
      removeFirst _ [] = []
      removeFirst c (c1:cs) = if c1==c then cs else c1:cs

      removeApostrophes = reverse . removeFirst '"' . reverse . removeFirst '"'


defaultFrameWidth = 20

frame n tape =  FramedTape tape n
