{-# LANGUAGE FlexibleInstances #-}
module Lecture10 where


import Comonad.Stream as Stream
import Control.Comonad
import Test.QuickCheck



prop_1 :: Int -> Int -> Bool
prop_1 x y =
  x+y == y+x

quicksort :: (Ord a) => [a]-> [a]
quicksort [] = []
quicksort (x:xs) = quicksort left ++ [x] ++ quicksort right
  where
    left = filter  (<=x) xs
    right = filter (>x) xs

prop_sort :: [Int] -> Bool
prop_sort list =
  case list of
    [] -> True
    _  -> quicksort list == min : quicksort shorterList
  where
    shorterList = deleteFirst min list
    min = minimum list

    deleteFirst _ [] = []
    deleteFirst a (x:xs) = if x == a then xs else x: deleteFirst a xs

x = generate (arbitrary :: Gen [Int])

data Tree a = Empty | Node (Tree a) a (Tree a)
  deriving Show


instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do -- arbitrary :: Gen (Tree a)
    left  <- arbitrary -- :: Gen (Tree a)
    value <- arbitrary -- arbitrary :Gen a
    right <- arbitrary -- :: Gen (Tree a)
    frequency [ (10, return Empty)
              , (9, return $ Node left value right)
              ]

y = generate (arbitrary :: Gen (Tree Int))
-- prop :: Tree Int -> Bool


-- prop_1 extend extract = id
prop_stream_1 :: Stream Int -> Bool
prop_stream_1 stream =
  extend extract stream == stream

instance Arbitrary a => Arbitrary (Stream a) where
  arbitrary = do
    NonEmpty list <- arbitrary
    return $ Stream.repeat list

prop_stream_2 :: Stream Bool -> (Stream Bool -> Bool) -> Bool
prop_stream_2 stream f =
  extract (extend f stream) == f stream

instance (CoArbitrary a) => CoArbitrary (Stream a) where
  coarbitrary stream = coarbitrary $ Stream.take 10 stream


instance Show (Stream Bool -> Bool) where
  show _ = ""

test :: IO ()
test = quickCheck prop_stream_1 >> quickCheck prop_stream_2



-- prawa komonadyczne:
-- extend extract      = id
-- extract . extend f  = f
-- extend f . extend g = extend (f . extend g)

