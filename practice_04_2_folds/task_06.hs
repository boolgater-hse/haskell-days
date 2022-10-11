data Tree a = Nil | Branch (Tree a) a (Tree a)
    deriving (Eq, Show)

newtype Preorder a = PreO (Tree a)
    deriving (Eq, Show)
newtype Postorder a = PostO (Tree a)
    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)
    deriving (Eq, Show)

tree :: Tree Integer
tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)

instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch left num right) = foldr f (f num (foldr f ini right)) left

instance Foldable Preorder where
    foldr f ini (PreO Nil) = ini
    foldr f ini (PreO (Branch left num right)) = f num (foldr f (foldr f ini (PreO right)) (PreO left))

instance Foldable Postorder where
    foldr f ini (PostO Nil) = ini
    foldr f ini (PostO (Branch left num right)) = foldr f (foldr f (f num ini) (PostO right)) (PostO left)

instance Foldable Levelorder where
    foldr f ini (LevelO Nil) = ini
    foldr f ini (LevelO tree@(Branch left num right)) = helper [tree] []
        where
            helper [] rest
                | null rest = ini
                | otherwise = helper (reverse rest) []
            helper (Nil : xs) rest = helper xs rest
            helper ((Branch left num right) : xs) rest = f num (helper xs (right:left:rest))
