data Triple a = Tr a a a
    deriving (Eq, Show)

instance Functor Triple where
    fmap f (Tr a b c) = Tr (f a) (f b) (f c)

instance Applicative Triple where
    pure a = Tr a a a
    (<*>) (Tr f1 f2 f3) (Tr a b c) = Tr (f1 a) (f2 b) (f3 c)

instance Foldable Triple where
    foldr f ini (Tr a b c) = f a (f b (f c ini))

instance Traversable Triple where
    traverse f (Tr a b c) = Tr <$> f a <*> f b <*> f c
