newtype Cmps f g x = Cmps { getCmps :: f (g x) }
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Cmps f g) where
    fmap f (Cmps a) = Cmps (fmap (fmap f) a)

instance (Applicative f, Applicative g) => Applicative (Cmps f g) where
    pure a = Cmps (pure (pure a))
    (<*>) f a = Cmps ((<*>) <$> getCmps f <*> getCmps a)

instance (Foldable f, Foldable g) => Foldable (Cmps f g) where
    foldMap f (Cmps x) = foldMap (foldMap f) x
