{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
import System.Win32 (xBUTTON1, COORD (yPos))
data LTree a = LLeaf a | LNode a (LTree a) (LTree a)
    deriving (Eq,Show)

data MTree a = MLeaf a | UNode a (MTree a) | BNode (MTree a) (MTree a)
    deriving (Eq,Show)

getLLeaves :: LTree a -> [a]
getLLeaves (LLeaf x) = [x]
getLLeaves (LNode _ l r) = getLLeaves l ++ getLLeaves r

getMLeaves :: MTree a -> [a]
getMLeaves (MLeaf x) = [x]
getMLeaves (UNode _ t) = getMLeaves t
getMLeaves (BNode l r) = getMLeaves l ++ getMLeaves r

maxLDepth :: LTree a -> Integer
maxLDepth (LLeaf x) = 0
maxLDepth (LNode _ l r) = 1 + max (maxLDepth l) (maxLDepth r)

maxMDepth :: MTree a -> Integer
maxMDepth (MLeaf x) = 0
maxMDepth (UNode _ t) = 1 + maxMDepth t
maxMDepth (BNode l r) = 1 + max (maxMDepth l) (maxMDepth r)

maxLTree :: LTree Integer -> Integer
maxLTree (LLeaf x) = x
maxLTree (LNode x l r) = maximum [x, (maxLTree l), (maxLTree r)]

maxMTree :: MTree Integer -> Integer
maxMTree (MLeaf x) = x
maxMTree (UNode x t) = max x (maxMTree t)
maxMTree (BNode l r) = max (maxMTree l) (maxMTree r)

uncoveredLeafL :: Integer -> LTree Integer -> Bool
uncoveredLeafL y (LLeaf x) = y == x
uncoveredLeafL y (LNode x l r) = if uncoveredLeafL y l || uncoveredLeafL y r then x /= y
                else uncoveredLeafL y l || uncoveredLeafL y r

uncoveredLeafM :: Integer -> MTree Integer -> Bool
uncoveredLeafM y (MLeaf x) = y ==x
uncoveredLeafM y (UNode x t) = if uncoveredLeafM y t  then x /= y
                else uncoveredLeafM y t
uncoveredLeafM y (BNode l r) = uncoveredLeafM y l || uncoveredLeafM y r

mapLTree :: (a -> b) -> LTree a -> LTree b
mapLTree f (LLeaf x) = LLeaf (f x)
mapLTree f (LNode x l r) = LNode (f x) (mapLTree f l) (mapLTree f r)

mapMTree :: (a -> b) -> MTree a -> MTree b
mapMTree f (MLeaf x) = MLeaf (f x)
mapMTree f (UNode x t) = UNode (f x) (mapMTree f t)
mapMTree f (BNode l r) = BNode (mapMTree f l) (mapMTree f r)

applyLfun :: LTree Integer -> LTree Integer
applyLfun (LLeaf x) = LLeaf (2^x^2 - x)
applyLfun (LNode x l r) = LNode (2^x^2 - x) (applyLfun l) (applyLfun r)

applyMfun :: MTree Integer -> MTree Integer
applyMfun (MLeaf x) = MLeaf (2^x^2 - x)
applyMfun (UNode x t) = UNode (2^x^2 - x) (applyMfun t)
applyMfun (BNode l r) = BNode (applyMfun l) (applyMfun r)

orMaybes :: Maybe a -> Maybe a -> Maybe a
orMaybes Nothing y = y
orMaybes x _ = x

findLTree :: (a -> Bool) -> LTree a -> Maybe a
findLTree f (LLeaf x) = if f x then Just x else Nothing
findLTree f (LNode x l r) = if f x then Just x else orMaybes (findLTree f l) (findLTree f r)

findMTree :: (a -> Bool) -> MTree a -> Maybe a
findMTree f (MLeaf x) = if f x then Just x else Nothing
findMTree f (UNode x t) = if f x then Just x else findMTree f t
findMTree f (BNode l r) = orMaybes (findMTree f l) (findMTree f r)

findLpali :: LTree String -> Maybe String
findLpali = findLTree (\x -> x == reverse x)

findMpali :: MTree String -> Maybe String
findMpali = findMTree (\x -> x == reverse x)

foldLTree :: (a -> b -> b -> b) -> (a -> b) -> LTree a -> b
foldLTree n l (LLeaf x) = l x
foldLTree n l (LNode x t1 t2) = n x (foldLTree n l t1) (foldLTree n l t2)

foldMTree :: (b -> b -> b) -> (a -> b -> b) -> (a -> b) -> MTree a -> b
foldMTree n u l (MLeaf x) = l x
foldMTree n u l (UNode x t) = u x (foldMTree n u l t)
foldMTree n u l (BNode t1 t2) = n (foldMTree n u l t1) (foldMTree n u l t2)

getLLeaves' :: LTree a -> [a]
getLLeaves' = foldLTree (\_ l r -> l ++ r) ((: []))

getMLeaves' :: MTree a -> [a]
getMLeaves' = foldMTree ((++)) (\_ t -> t) ((: []))

uncoveredLeafL' :: Integer -> LTree Integer -> Bool
uncoveredLeafL' y (LLeaf x) = y == x
uncoveredLeafL' y (LNode x l r) = if uncoveredLeafL' y l || uncoveredLeafL' y r then x /= y
                else uncoveredLeafL' y l || uncoveredLeafL' y r


uncoveredLeafM' :: Integer -> MTree Integer -> Bool
uncoveredLeafM' y (MLeaf x) = y ==x
uncoveredLeafM' y (UNode x t) = if uncoveredLeafM' y t  then x /= y
                else uncoveredLeafM' y t
uncoveredLeafM' y (BNode l r) = uncoveredLeafM' y l || uncoveredLeafM' y r

exLTree :: LTree Integer
exLTree = LNode 5 (LLeaf 4)
                  (LNode 3 (LNode 2 (LLeaf 5) (LLeaf 1))
                           (LLeaf 7))
exMTree :: MTree Integer
exMTree = BNode (UNode 5 (BNode (MLeaf 1) (MLeaf 10)))
                (BNode (UNode 3 (MLeaf 3)) (UNode 4 (MLeaf 3)))

