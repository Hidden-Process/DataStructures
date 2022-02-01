module AVLBiDictionary( BiDictionary
                      , empty
                      , isEmpty
                      , size
                      , insert
                      , valueOf
                      , keyOf
                      , deleteByKey
                      , deleteByValue
                      , toBiDictionary
                      , compose
                      , isPermutation
                      , orbitOf
                      , cyclesOf
                      ) where

import qualified DataStructures.Dictionary.AVLDictionary as D
import qualified DataStructures.Set.BSTSet               as S

import           Data.List                               (intercalate, nub,
                                                          (\\))
import           Data.Maybe                              (fromJust, fromMaybe,
                                                          isJust)
import           Test.QuickCheck


data BiDictionary a b = Bi (D.Dictionary a b) (D.Dictionary b a)

-- | Exercise a. empty, isEmpty, size

empty :: (Ord a, Ord b) => BiDictionary a b
empty = Bi (D.empty) (D.empty)

isEmpty :: (Ord a, Ord b) => BiDictionary a b -> Bool
isEmpty (Bi dk dv) = D.isEmpty dk

size :: (Ord a, Ord b) => BiDictionary a b -> Int
size (Bi dk dv) = D.size dk

-- | Exercise b. insert

insert :: (Ord a, Ord b) => a -> b -> BiDictionary a b -> BiDictionary a b
insert k v (Bi dk dv) | D.isDefinedAt k dk = Bi (D.insert k v (D.delete k dk)) (D.insert v k (D.delete (fromJust(D.valueOf k dk)) dv ))
                      | otherwise = Bi (D.insert k v dk) (D.insert v k dv)

-- | Exercise c. valueOf

valueOf :: (Ord a, Ord b) => a -> BiDictionary a b -> Maybe b
valueOf k (Bi dk dv) = D.valueOf k dk

-- | Exercise d. keyOf

keyOf :: (Ord a, Ord b) => b -> BiDictionary a b -> Maybe a
keyOf v (Bi dk dv) = D.valueOf v dv

-- | Exercise e. deleteByKey

deleteByKey :: (Ord a, Ord b) => a -> BiDictionary a b -> BiDictionary a b
deleteByKey k bi@(Bi dk dv) | D.isDefinedAt k dk = Bi (D.delete k dk) (D.delete (fromJust(valueOf k bi)) dv)
                            | otherwise = bi

-- | Exercise f. deleteByValue

deleteByValue :: (Ord a, Ord b) => b -> BiDictionary a b -> BiDictionary a b
deleteByValue v bi@(Bi dk dv) | D.isDefinedAt v dv = Bi (D.delete (fromJust(keyOf v bi)) dk) (D.delete v dv)
                              | otherwise = bi
                              
-- | Exercise g. toBiDictionary

toBiDictionary :: (Ord a, Ord b) => D.Dictionary a b -> BiDictionary a b
toBiDictionary dict | isInjective (D.values dict) =  insertRec (D.keys dict) empty
                    | otherwise = error "Diccionario no inyectivo"
                      where insertRec [] bd = bd
                            insertRec (x:xs) bd = insertRec xs (insert x (fromJust(D.valueOf x dict)) bd)
                         

isInjective :: (Ord a) => [a] -> Bool
isInjective [] = True
isInjective (x:xs) = if (elem x xs) then False else isInjective xs

-- | Exercise h. compose

compose :: (Ord a, Ord b, Ord c) => BiDictionary a b -> BiDictionary b c -> BiDictionary a c
compose (Bi dk1 dv1) (Bi dk2 dv2) = compose' (D.keys dv1) (D.keys dk2) empty
   where
      compose' [] (y:ys) dict = dict
      compose' (x:xs) [] dict = dict
      compose' (x:xs) (y:ys) dict | elem x (y:ys) = compose' xs (y:ys) (insert  (fromJust(D.valueOf x dv1)) (fromJust(D.valueOf x dk2)) dict)
                                  | otherwise = compose' xs (y:ys) dict 

-- | Exercise i. isPermutation

isPermutation :: Ord a => BiDictionary a a -> Bool
isPermutation (Bi dk dv) = D.keys dk == D.keys dv


-- |------------------------------------------------------------------------


-- | Exercise j. orbitOf

orbitOf :: Ord a => a -> BiDictionary a a -> [a]
orbitOf = undefined

-- | Exercise k. cyclesOf

cyclesOf :: Ord a => BiDictionary a a -> [[a]]
cyclesOf = undefined

-- |------------------------------------------------------------------------


instance (Show a, Show b) => Show (BiDictionary a b) where
  show (Bi dk dv)  = "BiDictionary(" ++ intercalate "," (aux (D.keysValues dk)) ++ ")"
                        ++ "(" ++ intercalate "," (aux (D.keysValues dv)) ++ ")"
   where
    aux kvs  = map (\(k,v) -> show k ++ "->" ++ show v) kvs
