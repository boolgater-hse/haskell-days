absDiff :: Num a => [a] -> [a]
absDiff [] = []
absDiff (x:xs)
    | null xs = absDiff xs
    | otherwise = abs (head xs - x) : absDiff xs
