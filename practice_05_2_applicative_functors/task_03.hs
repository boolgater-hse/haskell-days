data Tree a = Nil | Branch (Tree a) a (Tree a)
    deriving (Eq, Show)

t1 :: Tree Integer
t1 = Branch (Branch Nil 7 Nil) 2 Nil

t2 :: Tree Integer
t2 = Branch (Branch Nil 3 Nil) 4 (Branch Nil 5 Nil)

instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Branch left num right) = Branch (fmap f left) (f num) (fmap f right)

instance Applicative Tree where
    pure a = Branch (pure a) a (pure a)
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Branch fleft f fright) (Branch left num right) = Branch ((<*>) fleft left) (f num) ((<*>) fright right)
