{-# LANGUAGE InstanceSigs #-}

data NEList a = Single a | Cons a (NEList a)
    deriving (Eq, Show)

instance Functor NEList where
    fmap :: (a -> b) -> NEList a -> NEList b
    fmap f (Single x) = Single (f x)
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable NEList where
    foldMap :: Monoid m => (a -> m) -> NEList a -> m
    foldMap f (Single x) = f x
    foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable NEList where
    sequenceA :: Applicative f => NEList (f a) -> f (NEList a)
    sequenceA (Single x) = Single <$> x
    sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs
