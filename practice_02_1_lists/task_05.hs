-- Infinite counting list
nats :: [Integer]
nats = 1 : map (+1) nats

sublist :: Int -> Int -> [a] -> [a]
sublist n m [] = []
sublist n m (x:xs)
    | n < 0 = sublist 0 m (x:xs)
    | otherwise = take (m - n) (drop n (x:xs))
