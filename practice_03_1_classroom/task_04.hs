data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Show a => Show (Tree a) where
    show Leaf = "{}"
    show (Node left num right) = "<" ++ show left ++ show num ++ show right ++ ">"
