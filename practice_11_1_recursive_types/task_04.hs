import RT.Lib

instance Functor (T a) where
    fmap f Leaf = Leaf
    fmap f (Branch left num right) = Branch (f left) num (f right)

data T num kid = Leaf | Branch kid num kid
    deriving (Eq, Show)

type Tree a = Fix (T a)

phiTSum :: Algebra (T Integer) Integer
phiTSum Leaf = 0
phiTSum (Branch left num right) = left + num + right

treeSum :: Tree Integer -> Integer
treeSum = cata phiTSum
