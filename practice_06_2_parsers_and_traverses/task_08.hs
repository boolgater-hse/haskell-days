class Functor f => Monoidal f where
    unit :: f ()
    (*&*) :: f a -> f b -> f (a,b)

pure' :: Monoidal f => a -> f a
pure' a = a <$ unit

ap' :: Monoidal f => f (a -> b) -> f a -> f b
ap' a b = uncurry ($) <$> (a *&* b)
