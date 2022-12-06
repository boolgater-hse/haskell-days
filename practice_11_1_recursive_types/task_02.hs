import RT.Lib

instance Functor E where
    fmap f (Num a) = Num a
    fmap f (Add a b) = Add (f a) (f b)
    fmap f (Mult a b) = Mult (f a) (f b)

data E e = Num Int | Add e e | Mult e e
    deriving (Eq, Show)

type Expr = Fix E

phiE :: E Int -> Int
phiE (Num a) = a
phiE (Add a b) = a + b
phiE (Mult a b) = a * b

eval :: Expr -> Int
eval = cata phiE

phiEShow :: E String -> String
phiEShow (Num a) = show a
phiEShow (Add a b) = "(" ++ a ++ "+" ++ b ++ ")"
phiEShow (Mult a b) = "(" ++ a ++ "*" ++ b ++ ")"
