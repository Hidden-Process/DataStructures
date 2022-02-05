module Kruskal(kruskal, kruskals) where

import Data.Maybe(fromJust)
import qualified DataStructures.Dictionary.AVLDictionary as D
import qualified DataStructures.PriorityQueue.LinearPriorityQueue as Q
import DataStructures.Graph.DictionaryWeightedGraph

kruskal :: (Ord a, Ord w) => WeightedGraph a w -> [WeightedEdge a w]
kruskal graph = kruskal' pq dict []
  where
      pq   = foldr (Q.enqueue) (Q.empty) (edges graph)
      dict = foldr (\v d -> D.insert v v d) (D.empty) (vertices graph)
      kruskal' q d f | Q.isEmpty q = f 
                     | r1 == r2    = kruskal' (Q.dequeue q) d f
                     | otherwise   = kruskal' (Q.dequeue q) (D.insert r2 v1 d) (e:f)
                       where
                           r1 = representante v1 dict
                           r2 = representante v2 dict
                           e@(WE v1 w v2) = Q.first q

representante :: (Ord a) => a -> D.Dictionary a a -> a
representante v dict | v == w    = v
                     | otherwise = representante w dict
                        where
                            w    = fromJust(D.valueOf v dict)

-- Solo para evaluación continua / only for part time students
kruskals :: (Ord a, Ord w) => WeightedGraph a w -> [[WeightedEdge a w]]
kruskals = undefined
