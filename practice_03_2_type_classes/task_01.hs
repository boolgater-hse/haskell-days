newtype Matrix a = Matrix [[a]]

instance Show a => Show (Matrix a) where
    show (Matrix []) = "EMPTY"
    show (Matrix (x:xs))
        | null xs = show x
        | otherwise = show x ++ "\n" ++ show (Matrix xs)
