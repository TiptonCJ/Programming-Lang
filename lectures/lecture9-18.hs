data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Show, Eq)
data List a = Nil | Cons a (List a)
data RTree a  = RNode a (RTree a)

tree :: Tree Integer
tree = Node 5 (Node 17 Leaf Leaf)
              (Node 4 (Node 1 Leaf Leaf)
              (Node 2 Leaf Leaf))

data Comb = Var String | I | K | S | App Comb Comb

addList :: List Integer -> Integer
addList (Nil) = 0
addList (Cons x xs) = x + addList xs

addTree :: Tree Integer -> Integer
addTree (Leaf) = 0
addTree (Node x y z) = x + addTree y + addTree z

exTree :: Tree Integer
exTree = Node 3 (Node 2 Leaf Leaf)
                (Node 5 (Node 1 Leaf Leaf)
                        (Node 7 Leaf Leaf))

doubleTree :: Tree Integer -> Tree Integer
doubleTree Leaf = Leaf
doubleTree (Node x t1 t2) = Node (2*x) (doubleTree t1) (doubleTree t2)

showTree :: Tree Integer -> Tree String
showtree Leaf = Leaf
showTree (Node x t1 t2) = Node (show x) (showTree t1) (showTree t2)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node x t1 t2) = Node (f x) (mapTree f t1) (mapTree f t2)

concatTree :: Tree String -> String
concatTree Leaf = ""
concatTree (Node x t1 t2) = concatTree t1 ++ x ++ concatTree t2

comb :: Comb
comb = App (App (App I (App S K)) K ) (Var "z")

getVars :: Comb -> [String]
getVars (Var x) = [x]
getVars I = []
getVars K = []
getVars S = []
getVars (App c1 c2) = getVars c1 ++ getVars c2

countConst :: Comb -> Integer
countConst (Var x) = 0
countConst (App c1 c2) = countConst c1 + countConst c2
countConst _ = 1

reduce :: Comb -> Comb
reduce (App I c) = c
reduce (App (App K c1) c2) = c1
reduce (App (App (App S x) y ) z ) = App (App x y) (App y z)