module DataStructures.Queue.TwoStacksQueue
  ( Queue
  , empty
  , isEmpty
  , enqueue
  , first
  , dequeue
  ) where

import Data.List(intercalate)
import Test.QuickCheck
import qualified DataStructures.Stack.LinearStack as S

data Queue a = TSQ (S.Stack a) (S.Stack a)

-- forces this invariant: first stack is only empty if queue is empty
mkValid :: S.Stack a -> S.Stack a -> Queue a
mkValid s1 s2 | S.isEmpty s1 = TSQ (reverseStack s2) S.empty
              | otherwise    = TSQ s1 s2

reverseStack :: S.Stack a -> S.Stack a
reverseStack s = aux s S.empty
  where
    aux s1 s2 | S.isEmpty s1 = s2
              | otherwise    = aux (S.pop s1) (S.push (S.top s1) s2)

empty :: Queue a
empty = TSQ S.empty S.empty

isEmpty :: Queue a -> Bool
isEmpty (TSQ s1 s2) = S.isEmpty s1

enqueue :: a -> Queue a -> Queue a
enqueue x q@(TSQ s1 s2) = mkValid s1 (S.push x s2)

dequeue :: Queue a -> Queue a
dequeue q@(TSQ s1 s2) | isEmpty q = error "Dequeue on Empty Queue"
                      | otherwise   = mkValid (S.pop s1) s2

first :: Queue a -> a
first q@(TSQ s1 s2) | isEmpty q = error "First on Empty Queue"
                    | otherwise   = S.top s1


-- =====================================================================
-- ======================= NO TOCAR DE AQUI PARA ABAJO =================
-- Showing a queue
instance (Show a) => Show (Queue a) where
  show q = "TwoStacksQueue(" ++ intercalate "," (aux q) ++ ")"
    where
        aux q
          | isEmpty q = []
          | otherwise = show (first q) : aux (dequeue q)

-- Queues equality
instance (Eq a) => Eq (Queue a) where
  q1 == q2
    | e1 && e2         = True
    | not e1 && not e2 = first q1 == first q2 && dequeue q1 == dequeue q2
    | otherwise        = False
    where
      e1 = isEmpty q1
      e2 = isEmpty q2

-- This instance is used by QuickCheck to generate random queues
instance (Arbitrary a) => Arbitrary (Queue a) where
    arbitrary = do
      xs <- listOf arbitrary
      return (foldr enqueue empty xs)
