data OddC a = Un a | Bi a a (OddC a)
    deriving (Eq, Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Bi x1 x2 x3) y z = Bi x1 x2 (concat3OC x3 y z)
concat3OC (Un x) (Bi y1 y2 y3) z = Bi x y1 (concat3OC (Un y2) y3 z)
concat3OC (Un x) (Un y) z = Bi x y z
