-------------------------------------------------------------------------------
-- Student's name:
-- Student's group:
--
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Graph.EulerianCycle(isEulerian, eulerianCycle) where

import DataStructures.Graph.Graph
import Data.List

--H.1)
isEulerian :: Eq a => Graph a -> Bool
isEulerian g = isEulerian' (vertices g)
                 where 
                   isEulerian'[] = True
                   isEulerian' (x:xs) = even (degree g x) && isEulerian' xs

isEulerian'' g = all (\v -> even (degree g v)) (vertices g) 

-- H.2)
remove :: (Eq a) => Graph a -> (a,a) -> Graph a
remove g (v,u) = aux g' (vertices g')
  where
   g' = deleteEdge g (v,u)
   aux gr [] = gr
   aux gr (w:ws) | degree gr w == 0 = aux (deleteVertex gr w) ws
                 | otherwise = aux gr ws

-- H.3)
extractCycle :: (Eq a) => Graph a -> a -> (Graph a, Path a)
extractCycle g v0 = aux g v0 (head (successors g v0)) [v0]
  where
    aux g v u ps | u == v0   = (g',u:ps)
                 | otherwise = aux g' u (head (successors g' u)) (u:ps)
                     where
                        g' = remove g (v,u)

-- H.4)
connectCycles :: (Eq a) => Path a -> Path a -> Path a
connectCycles [] ys = ys
connectCycles (x:xs) (y:ys) | x == y = (y:ys)++xs
                            | otherwise = x:connectCycles xs (y:ys) 

-- H.5)
vertexInCommon :: Eq a => Graph a -> Path a -> a
vertexInCommon g (x:xs) | elem x (vertices g) = x
                        | otherwise = vertexInCommon g xs

-- H.6) 
eulerianCycle :: Eq a => Graph a -> Path a
eulerianCycle g | isEulerian g = aux g (head (vertices g)) []
                | otherwise    = error "Not Eulerian Graph"
                   where
                     aux g v ps | isEmpty gr' = ps''
                                | otherwise   = aux gr' u ps''
                                    where
                                      (gr',ps') = extractCycle g v
                                      ps''      = connectCycles ps ps'
                                      u         = vertexInCommon gr' ps''


