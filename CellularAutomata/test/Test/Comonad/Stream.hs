{-# LANGUAGE FlexibleInstances #-}
module Test.Comonad.Stream where

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Modifiers
import qualified Comonad.Stream as S
import Control.Comonad

-- Typeclass Arbitrary stosowany jest przez QuickCheck do generowania
-- przykładów, które są sprawdzane.
instance Arbitrary a => Arbitrary (S.Stream a) where
  arbitrary = do
    NonEmpty list <- arbitrary
    return $ S.repeat list

-- Typeclass CoArbitrary potrzebne jest aby QuickCheck umiał generować
-- funkcje typu S.Stream a -> b
instance CoArbitrary a => CoArbitrary (S.Stream a) where
  coarbitrary stream  = coarbitrary $ S.take 10 stream

-- prawa komonadyczne:
-- extend extract      = id
prop1 :: S.Stream Bool -> Bool
prop1 stream =
  extend extract stream == stream

-- extract . extend f  = f
prop2 :: S.Stream Bool
      -> (S.Stream Bool -> Bool)
      -> Bool
prop2 stream f =
  extract (extend f stream) == f stream

-- QuickCheck wymaga instancji Show dla typów, które generuje.
-- Co oznacza, że musimy dopisać jeszcze to.
-- (Za bardzo się nie staramy tutaj akurat)
instance (Show (S.Stream Bool -> Bool)) where
  show f = ""

-- extend f . extend g = extend (f . extend g)
prop3 :: S.Stream Bool
      -> (S.Stream Bool -> Bool)
      -> (S.Stream Bool -> Bool)
      -> Bool
prop3 stream f g =
  (extend f . extend g) stream == extend (f . extend g) stream

-- funkcja do testowania wszystkich stwierdzeń
test = putStrLn "Stream comonad test suite"
       >> quickCheck prop1
       >> quickCheck prop2
       >> quickCheck prop3
