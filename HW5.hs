data LTree a = LLeaf a | LNode a (LTree a) (LTree a)
    deriving (Eq,Show)

data MTree a = MLeaf a | UNode a (MTree a) | BNode (MTree a) (MTree a)
    deriving (Eq,Show)

