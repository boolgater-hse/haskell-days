class Functor f => Monoidal f where
    unit :: f ()
    (*&*) :: f a -> f b -> f (a,b)

instance Monoidal Maybe where
    unit = pure ()
    (*&*) a b = (,) <$> a <*> b

instance Monoid s => Monoidal ((,) s) where
    unit = pure ()
    (*&*) a b = (,) <$> a <*> b

instance Monoidal ((->) e) where
    unit = pure ()
    (*&*) a b = (,) <$> a <*> b
