update :: (Eq a) => (a,b) -> [(a,b)] -> [(a,b)]
update (x,y) [] = [(x,y)]
update (x,y) ((k,v):list) = if x == k then (x,y) : list else (k,v) : update (x,y) list

supset :: (Eq a) => [a] -> [a] -> Bool
supset x [] = True
supset xs (y:ys) = if y `elem` xs then supset xs ys else False

findRoot :: (Integer -> Integer) -> [Integer] -> Maybe Integer
findRoot f [] = Nothing
findRoot f (x:xs) = if f x == 0 then Just x else findRoot f xs

squashAll :: [String] -> [String]
squashAll = map $ filter (/= ' ')

concatPairs :: [(String,String)] -> [String]
concatPairs = map (\(x,y) -> x ++ y) 

noDups :: (Eq a) => [a] -> [a]
noDups = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) []

data BTree a = BLeaf a | UNode (BTree a) | BNode a (BTree a) (BTree a)
myTree :: BTree Integer
myTree = BNode 3 (UNode (BLeaf 5))
                 (BNode 4 (BLeaf 1) (UNode (BLeaf (-3))))

sumLeaves :: BTree Integer-> Integer
sumLeaves (BLeaf x) = x
sumLeaves (UNode x) = sumLeaves x
sumLeaves (BNode x y z) = sumLeaves y + sumLeaves z

mapBTree :: (a -> b) -> BTree a -> BTree b
mapBTree f (BLeaf x) = BLeaf (f x)
mapBTree f (UNode t) = UNode (mapBTree f t)
mapBTree f (BNode x l r) = BNode (f x) (mapBTree f l) (mapBTree f r)

checkBNodes :: (Integer -> Bool) -> BTree Integer -> Bool
checkBNodes f (BLeaf x) = False
checkBNodes f (UNode t) = checkBNodes f t
checkBNodes f (BNode x l r) = f x || checkBNodes f l || checkBNodes f r

