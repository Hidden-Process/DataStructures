module TwoListsDoubleEndedQueue
   ( DEQue
   , empty
   , isEmpty
   , first
   , last
   , addFirst
   , addLast
   , deleteFirst
   , deleteLast
   ) where

import Prelude hiding (last)
import Data.List(intercalate)
import Test.QuickCheck

data DEQue a = DEQ [a] [a]

-- Complexity: O(1)
empty :: DEQue a
empty = DEQ [] []

-- Complexity: O(1)
isEmpty :: DEQue a -> Bool
isEmpty (DEQ xs ys) = null xs && null ys

-- Complexity: O(1)
addFirst :: a -> DEQue a -> DEQue a
addFirst x (DEQ xs ys) = DEQ (x:xs) ys

-- Complexity: O(1)
addLast :: a -> DEQue a -> DEQue a
addLast x (DEQ xs ys) = DEQ xs (x:ys)

-- Complexity: O(1)
first :: DEQue a -> a
first (DEQ [] []) = error("first on empty DEQ")
first (DEQ [] ys) = head (reverse ys)
first (DEQ (x:xs) ys) = x

-- Complexity: O(1)
last :: DEQue a -> a
last (DEQ [] []) = error("last on empty DEQ")
last (DEQ xs []) = head (reverse xs)
last (DEQ xs (y:ys)) = y

-- Complexity: O(1)
deleteFirst :: DEQue a -> DEQue a
deleteFirst (DEQ [] []) = empty
deleteFirst (DEQ [] (x:[])) = empty
deleteFirst (DEQ (x:xs) ys) = DEQ xs ys  
deleteFirst (DEQ [] ys) = deleteFirst (DEQ (reverse hs') ts')
  where
     (ts', hs') = splitAt half ys
       where
          half | length ys `mod` 2 == 0 = length ys `div` 2
               | otherwise              = (length ys `div` 2) + 1

-- Complexity: O(1)
deleteLast :: DEQue a -> DEQue a
deleteLast (DEQ [] []) = empty
deleteLast (DEQ xs (y:ys)) = DEQ xs ys
deleteLast (DEQ xs []) = deleteLast(DEQ hs' (reverse ts'))
  where
     (hs',ts') = splitAt half xs
       where
         half | length xs `mod` 2 == 0 = length xs `div` 2
              | otherwise              = (length xs `div` 2) + 1

instance (Show a) => Show (DEQue a) where
   show q = "TwoListsDoubleEndedQueue(" ++ intercalate "," [show x | x <- toList q] ++ ")"

toList :: DEQue a -> [a]
toList (DEQ xs ys) =  xs ++ reverse ys

instance (Eq a) => Eq (DEQue a) where
   q == q' =  toList q == toList q'

instance (Arbitrary a) => Arbitrary (DEQue a) where
   arbitrary =  do
      xs <- listOf arbitrary
      ops <- listOf (oneof [return addFirst, return addLast])
      return (foldr id empty (zipWith ($) ops xs))
