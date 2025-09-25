data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Show, Eq)
data List a = Nil | Cons a (List a)
data RTree a  = RNode a (RTree a)

tree :: Tree Integer
tree = Node 5 (Node 17 Leaf Leaf)
              (Node 4 (Node 1 Leaf Leaf)
              (Node 2 Leaf Leaf))

data Comb = Var String | I | K | S | App Comb Comb


