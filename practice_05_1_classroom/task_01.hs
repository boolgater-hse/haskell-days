{-# LANGUAGE InstanceSigs #-}

data E l r = L l | R r
    deriving (Eq, Show)

instance Functor (E l) where
    fmap :: (a -> b) -> E l a -> E l b
    fmap _ (L l) = L l
    fmap f (R a) = R (f a)

instance Applicative (E l) where
    pure :: a -> E l a
    pure a = R a

    (<*>) :: E l (a -> b) -> E l a -> E l b
    (<*>) (R f) (R a) = R (f a)
    (<*>) (L e) _ = L e
    (<*>) _ (L e) = L e
