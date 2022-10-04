data Tree a = Leaf | Node (Tree a) a (Tree a) 
    deriving Show

instance Functor Tree where
    fmap func Leaf = Leaf
    fmap func (Node left num right) = Node (fmap func left) (func num) (fmap func right)
