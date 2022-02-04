module AVL 
  ( 
    Weight
  , Capacity
  , AVL (..)
  , Bin
  , emptyBin
  , remainingCapacity
  , addObject
  , maxRemainingCapacity
  , height
  , nodeWithHeight
  , node
  , rotateLeft
  , addNewBin
  , addFirst
  , addAll
  , toList
  , linearBinPacking
  , seqToList
  , addAllFold
  ) where

type Capacity = Int
type Weight= Int

data Bin = B Capacity [Weight] 

data AVL = Empty | Node Bin Int Capacity AVL AVL deriving Show


emptyBin :: Capacity -> Bin
emptyBin cap = B cap []


remainingCapacity :: Bin -> Capacity
remainingCapacity (B cap _) = cap


addObject :: Weight -> Bin -> Bin
addObject obj (B cap xs) | obj > cap = error "The object it's too heavy to fit in the Bin"
                         | otherwise = B (cap-obj) (obj:xs)


maxRemainingCapacity :: AVL -> Capacity
maxRemainingCapacity Empty = 0
maxRemainingCapacity (Node bin he cap lt rt) = cap


height :: AVL -> Int
height Empty = 0
height (Node bin he cap lt rt) = he
 

nodeWithHeight :: Bin -> Int -> AVL -> AVL -> AVL
nodeWithHeight bin@(B cap xs) he lt rt = Node bin he (maxCap lt rt) lt rt
  where 
    maxCap lt rt = max (maxRemainingCapacity lt) (maxRemainingCapacity rt)


node :: Bin -> AVL -> AVL -> AVL
node bin@(B cap xs) lt rt = Node bin (setHeight lt rt) (maxCap lt rt) lt rt
  where
    setHeight lt rt = max (height lt) (height rt) + 1
    maxCap lt rt    = max (maxRemainingCapacity lt) (maxRemainingCapacity rt)


rotateLeft :: Bin -> AVL -> AVL -> AVL
rotateLeft c l (Node x he cap r1 r2) = node x (node c l r1) r2

addNewBin :: Bin -> AVL -> AVL
addNewBin bin Empty = Node bin 1 (remainingCapacity bin) Empty Empty
addNewBin bin (Node b h cap lt rt) | height rt - height lt > 1 = rotateLeft b lt (addNewBin bin rt) 
                                   | otherwise = node b lt (addNewBin bin rt)
 
addFirst :: Capacity -> Weight -> AVL -> AVL
addFirst cap obj Empty = addNewBin (addObject obj (emptyBin cap)) Empty
addFirst cap obj tree@(Node bin h c lt rt) | obj > c   = addNewBin (addObject obj (emptyBin cap)) tree
                                           | maxRemainingCapacity lt >= obj = Node bin h c (addFirst cap obj lt) rt
                                           | c >= obj  =  Node (addObject obj bin) h (c-obj) lt rt
                                           | otherwise =  Node bin h c lt (addFirst cap obj rt) 

addAll:: Capacity -> [Weight] -> AVL
addAll cap xs = addAll' cap xs Empty
  where
    addAll' cap [] avl = avl
    addAll' cap (x:xs) avl = addAll' cap xs (addFirst cap x avl)

toList :: AVL -> [Bin]
toList Empty = []
toList (Node bin h cap lt rt) = toList lt ++ [bin] ++ toList rt

{-
	SOLO PARA ALUMNOS SIN EVALUACION CONTINUA
  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
 -}

data Sequence = SEmpty | SNode Bin Sequence deriving Show  

linearBinPacking:: Capacity -> [Weight] -> Sequence
linearBinPacking cap xs = linearBinPacking' cap xs SEmpty
  where
    linearBinPacking' cap [] seq         = seq
    linearBinPacking' cap (x:xs)  SEmpty = linearBinPacking' cap xs (SNode (B (cap-x) [x]) SEmpty)
    linearBinPacking' cap (x:xs) (SNode bin@(B c ys) seq) | x <= c  = linearBinPacking' cap xs (SNode (addObject x bin) seq)
                                                          | otherwise = SNode bin (linearBinPacking' cap (x:xs) seq)

seqToList:: Sequence -> [Bin]
seqToList SEmpty = []
seqToList (SNode bin seq) = [bin] ++ seqToList seq

addAllFold:: [Weight] -> Capacity -> AVL 
addAllFold _ _ = undefined


{- No modificar. Do not edit -}

objects :: Bin -> [Weight]
objects (B _ os) = reverse os

  
instance Show Bin where
  show b@(B c os) = "Bin("++show c++","++show (objects b)++")"