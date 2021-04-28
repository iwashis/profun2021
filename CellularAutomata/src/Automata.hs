module Automata where

import Comonad.Tape
import Comonad.Stream
import Control.Comonad


xor x y = (not x || not y) && ( x || y )


-- bardziej zaawansowana
rule30 :: Tape Bool -> Bool
rule30 (Tape (Stream x _) y (Stream z _)) = x `xor` (y || z)


-- zasada 30 związana z trójkątem Sierpińskiego
rule90 :: Tape Bool -> Bool
rule90 (Tape (Stream x _) _ (Stream z _)) = x `xor` z


applyRule :: Tape a -> (Tape a -> a) -> [Tape a]
applyRule initial rule = 
  initial : ( (rule `extend`) <$> applyRule initial rule )



