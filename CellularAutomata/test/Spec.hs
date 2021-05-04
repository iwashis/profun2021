{-# LANGUAGE DeriveFunctor #-}
import qualified Test.Comonad.Stream as Stream

import Test.QuickCheck
import Test.QuickCheck.Modifiers

-- pokaz sił quickchecka!
--
quicksort :: (Ord a) => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = quicksort left ++ [x] ++ quicksort right
  where
    left  = filter (<=x) xs
    right = filter (>x)  xs

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

-- troszkę ogólniej

x = generate (arbitrary  :: Gen Int)

data Tree a = Empty | Node (Tree a) a (Tree a)
  deriving (Functor,Show, Eq)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    left  <-  arbitrary
    right <- arbitrary
    value <- arbitrary
    frequency [ (10, return Empty)
              , (9, return $ Node left value right)
              ]


y = generate (arbitrary :: Gen (Tree Int))


main :: IO ()
main = Stream.test
