module Practica2 where

import Test.QuickCheck
import Data.List

-------------------------------------------------------------------------------
-- Ejercicio 4
-------------------------------------------------------------------------------

distintos :: Eq a => [a] -> Bool
distintos [] = True
distintos (x:xs) | elem x xs = False
                 | otherwise = distintos xs

-------------------------------------------------------------------------------
-- Ejercicio 11
-------------------------------------------------------------------------------

take' :: Int -> [a] -> [a]
take' n xs = [ x | (p,x) <- zip [1..n] xs]

drop' :: Int -> [a] -> [a]
drop' n xs = [x | (p,x) <- zip [0..length(xs)]  xs, p>=n] 

p1 n xs = n>0 ==> take' n xs ++ drop' n xs == xs 

-------------------------------------------------------------------------------
-- Ejercicio 13
-------------------------------------------------------------------------------

desconocida :: (Ord a) => [a] -> Bool
desconocida xs = and [ x<=y | (x,y) <- zip xs (tail xs) ]

{- Qué hace?

R: Esta funcion nos indica si una lista esta ordenada utilizando la funcion 
zip para ir creando una tupla entre un elemento de la lista y el siguiente 
utilizando para ello la funcion tail, que quita el primer elemto de la lista.
Y comprueba que x es menor que y eso devuelve una lista de booleanos que utilizando
la funcion and del principio nos devolvera true si todos esos pares intermedios
estaban ordenados.

-}

-------------------------------------------------------------------------------
-- Ejercicio 14
-------------------------------------------------------------------------------

-- apartados a, b, e y f
-- a)
inserta :: (Ord a) => a -> [a] -> [a]
inserta x s = takeWhile(<x) s ++  x :  dropWhile(<x) s


-- b)
inserta' :: (Ord a ) => a -> [a] -> [a]
inserta' x [] = [x]
inserta' x (y:ys) | x <= y  = (x:y:ys)
                  | otherwise = y:(inserta' x ys)

-- e)

ordena :: (Ord a) => [a] -> [a]
ordena xs = foldr inserta [] xs

-- f)  Utiliza para ello la función sorted definida en las transarencias

p2 xs = sort(xs) == ordena xs

-------------------------------------------------------------------------------
-- Ejercicio 22
-------------------------------------------------------------------------------

binarios ::Integer -> [String]
binarios 0 = [""]
binarios 1 = ["0","1"]
binarios x | x > 1 =  sort(map (++"0") (binarios(x-1)) ++ map (++"1") (binarios(x-1)))

-------------------------------------------------------------------------------
-- Ejercicio 34
-------------------------------------------------------------------------------

type Izdo = Double
type Dcho = Double
type Epsilon = Double
type Función = Double -> Double
biparticion :: Función -> Izdo -> Dcho -> Epsilon -> Double

biparticion f a b epsilon
  | long   <= epsilon  = c
  | (f c)  <= epsilon  = c 
  | a < 0              = biparticion f a c epsilon
  | b < 0              = biparticion f c b epsilon
  where
      long = b - a
      c = (a+b)/2


-------------------------------------------------------------------------------
-- Lista de ejercicios extra. Ejercicio [lista de pares] 
-------------------------------------------------------------------------------

cotizacion :: [(String, Double)]
cotizacion = [("apple", 116), ("intel", 35), ("google", 824), ("nvidia", 67)]

-- buscarRec

buscarRec :: (Eq a) => a -> [(a,b)] -> [b]
buscarRec x [] = []
buscarRec x (y:ys) | x == fst y = [snd y] 
                   | otherwise  = buscarRec x ys

-- buscarC

buscarC :: (Eq a) => a -> [(a,b)] -> [b]
buscarC x xs = [b | (a,b) <- xs, x == a]

-- buscarP

buscarP :: (Eq a) => a -> [(a,b)] -> [b]
buscarP x xs = undefined 

-- valorCartera. DIFICIL

-------------------------------------------------------------------------------
-- Lista de ejercicios extra. Ejercicio [mezcla]
-------------------------------------------------------------------------------
-- mezcla

mezcla :: (Ord a) => [a] -> [a] -> [a]
mezcla [] [] = []
mezcla [] ys = ys
mezcla xs [] = xs
mezcla (x:xs) (y:ys) | x <= y = x : mezcla xs (y:ys)
                     | otherwise = y : mezcla (x:xs) ys

-------------------------------------------------------------------------------
-- Lista de ejercicios extra. Ejercicio [agrupar]
-------------------------------------------------------------------------------
-- agrupar. DIFICIL
