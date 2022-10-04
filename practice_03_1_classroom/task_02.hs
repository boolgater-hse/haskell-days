data Tree a = Leaf | Node (Tree a) a (Tree a) 
    deriving Show

instance Eq a => Eq (Tree a) where
    Leaf == Leaf = True
    (Node left num right) == (Node left' num' right') = num == num' &&
                                                        bfs left == bfs left' &&
                                                        bfs right == bfs right'
    _ == _ = False

bfs :: Tree a -> [a]
bfs tree = helper [tree]
    where
        helper :: [Tree a] -> [a]
        helper [] = []
        helper (Leaf : xs) = helper xs
        helper (Node left num right : xs) = num : helper (xs ++ [left, right])
