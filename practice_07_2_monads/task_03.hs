factor2 :: Integer -> [(Integer, Integer)]
factor2 n = do
    concat ([[(a, b) | a <= b && a * b == n] | a <- [1..floor (sqrt (fromIntegral n + 1))], let b = n `div` a])
