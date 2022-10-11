{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

newtype Cmps f g x = Cmps { getCmps :: f (g x) }

instance (Functor f, Functor g) => Functor (Cmps f g) where
    fmap :: (a -> b) -> Cmps f g a -> Cmps f g b
    fmap f (Cmps x) = Cmps (fmap (fmap f) x)
