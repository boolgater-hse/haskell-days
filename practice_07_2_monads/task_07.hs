data OddC a = Un a | Bi a a (OddC a)
    deriving (Eq, Show)

instance Functor OddC where
    fmap f (Un x) = Un $ f x
    fmap f (Bi x y z) = Bi (f x) (f y) (f <$> z)

instance Applicative OddC where
    pure = Un

    (Un f) <*> (Un v) = Un (f v)
    (Un f) <*> (Bi v1 v2 v3) = Bi (f v1) (f v2) (f <$> v3)
    (Bi f1 f2 f3) <*> (Un v) = Bi (f1 v) (f2 v) (f3 <*> Un v)
    (Bi f1 f2 f3) <*> (Bi v1 v2 v3) = concat3OC a b c
        where
            a = Bi (f1 v1) (f1 v2) (f1 <$> v3)
            b = Bi (f2 v1) (f2 v2) (f2 <$> v3)
            c = concatOC (Bi (f3 <*> Un v1) (f3 <*> Un v2) (Un $ f3 <*> v3))

instance Monad OddC where
    (Un l) >>= f = f l
    (Bi x y z) >>= f = concat3OC a b c
        where
            a = f x
            b = f y
            c = z >>= f

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x) = x
concatOC (Bi (Un x) (Un y) z) = Bi x y (concatOC z)
concatOC (Bi (Un x) (Bi y1 y2 y3) z) = Bi x y1 (concatOC (Bi (Un y2) y3 z))
concatOC (Bi (Bi x1 x2 x3) y z) = Bi x1 x2 (concatOC (Bi x3 y z))

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Bi x1 x2 x3) y z = Bi x1 x2 (concat3OC x3 y z)
concat3OC (Un x) (Bi y1 y2 y3) z = Bi x y1 (concat3OC (Un y2) y3 z)
concat3OC (Un x) (Un y) z = Bi x y z
