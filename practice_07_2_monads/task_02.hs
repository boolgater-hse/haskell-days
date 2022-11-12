lookups :: (Eq k) => k -> [(k,v)] -> [v]
lookups x [] = []
lookups x (y:ys)
    | x == fst y = snd y : lookups x ys
    | otherwise = lookups x ys
