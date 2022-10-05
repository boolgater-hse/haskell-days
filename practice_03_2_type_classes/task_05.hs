comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb _ [] = []
comb n (x : xs) = map (x :) (comb (n - 1) xs) ++ comb n xs
