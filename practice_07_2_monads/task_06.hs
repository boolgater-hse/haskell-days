data OddC a = Un a | Bi a a (OddC a)
    deriving (Eq, Show)

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x) = x
concatOC (Bi (Un x) (Un y) z) = Bi x y (concatOC z)
concatOC (Bi (Un x) (Bi y1 y2 y3) z) = Bi x y1 (concatOC (Bi (Un y2) y3 z))
concatOC (Bi (Bi x1 x2 x3) y z) = Bi x1 x2 (concatOC (Bi x3 y z))
