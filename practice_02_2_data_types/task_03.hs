import Control.Exception (assert)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

treeSum :: Tree Integer -> Integer
treeSum Leaf = 0
treeSum (Node left num right) = num + treeSum left + treeSum right

treeHeight :: Tree a -> Int
treeHeight Leaf = 0
treeHeight (Node left num right) = let go Leaf = 0; go (Node left num right) = 1 + go left + go right in
    go left + go right

main :: IO ()
main = do
    let tree = Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 (Node Leaf 4 Leaf)
    assert (treeSum tree == 10) putStrLn "nice1"
    assert (treeHeight tree == 3) putStrLn "nice2"
