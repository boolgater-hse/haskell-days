{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import RT.Lib

instance Functor E where
    fmap f (Num a) = Num a
    fmap f (Add a b) = Add (f a) (f b)
    fmap f (Mult a b) = Mult (f a) (f b)

data E e = Num Int | Add e e | Mult e e
    deriving (Eq, Show)

type Expr = Fix E

phiEShowS :: E ShowS -> ShowS
phiEShowS (Num a) = shows a
phiEShowS (Add a b) = showString "+ " . a . showString " " . b
phiEShowS (Mult a b) = showString "* " . a . showString " " . b

type Stack = [Int]

push :: Int -> Stack -> Stack
push a as = a : as

add :: Stack -> Stack
add (a : b : cs) = (b + a) : cs

mult :: Stack -> Stack
mult (a : b : cs) = (b * a) : cs

phiE' :: E (Stack -> Stack) -> Stack -> Stack
phiE' (Num a) = push a
phiE' (Add a b) = add . a . b
phiE' (Mult a b) = mult . a . b

eval' :: Expr -> Stack -> Stack
eval' = cata phiE'
