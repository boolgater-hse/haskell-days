data Tree a = Nil | Branch (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Branch left num right) = Branch (fmap f left) (f num) (fmap f right)

instance Applicative Tree where
    pure a = Branch (pure a) a (pure a)
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Branch fleft f fright) (Branch left num right) = Branch ((<*>) fleft left) (f num) ((<*>) fright right)

instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch left num right) = foldr f (f num (foldr f ini right)) left

instance Traversable Tree where
    traverse f Nil = pure Nil
    traverse f (Branch left num right) = flip . Branch <$> traverse f left <*> traverse f right <*> f num
