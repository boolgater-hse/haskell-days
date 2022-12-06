import RT.Lib

instance Functor (T a) where
    fmap f Leaf = Leaf
    fmap f (Branch left num right) = Branch (f left) num (f right)

data T num kid = Leaf | Branch kid num kid
    deriving (Eq, Show)

type Tree a = Fix (T a)

phiTInorder :: Algebra (T a) [a] -- T a [a] -> [a]
phiTInorder Leaf = []
phiTInorder (Branch left num right) = left ++ [num] ++ right

tree2listInorder :: Tree a -> [a]
tree2listInorder = cata phiTInorder

psiTBST :: Ord a => Coalgebra (T a) [a] -- [a] -> T a [a] 
psiTBST [] = Leaf
psiTBST (num:xs) = Branch left num right
    where
        left = filter (<= num) xs
        right = filter (> num) xs

list2BST :: Ord a => [a] -> Tree a
list2BST = ana psiTBST

sort :: Ord a => [a] -> [a]
sort = hylo phiTInorder psiTBST
