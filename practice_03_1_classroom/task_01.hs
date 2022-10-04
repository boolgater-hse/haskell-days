data Tree a = Leaf | Node (Tree a) a (Tree a) 
    deriving (Show)

elemTree :: Eq a => a -> Tree a -> Bool
elemTree e tree = e `elem` bfs tree

bfs :: Tree a -> [a]
bfs tree = helper [tree]
    where
        helper :: [Tree a] -> [a]
        helper [] = []
        helper (Leaf : xs) = helper xs
        helper (Node left num right : xs) = num : helper (xs ++ [left, right])
