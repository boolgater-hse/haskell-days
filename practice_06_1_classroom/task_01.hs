{-# LANGUAGE InstanceSigs #-}

data Result a = Ok a | Error String
    deriving (Eq, Show)

instance Functor Result where
    fmap :: (a -> b) -> Result a -> Result b
    fmap _ (Error qq) = Error qq
    fmap f (Ok a) = Ok (f a)

instance Foldable Result where
    foldMap :: Monoid m => (a -> m) -> Result a -> m
    foldMap _ (Error _) = mempty
    foldMap f (Ok a) = f a

instance Traversable Result where
    traverse :: Applicative f => (a -> f b) -> Result a -> f (Result b)
    traverse f (Ok a) = Ok <$> f a
    traverse f (Error e) = pure (Error e)
