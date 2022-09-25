-- Infinite counting list
nats :: [Integer]
nats = 1 : map (+1) nats

movingLists :: Int -> [a] -> [[a]]
movingLists _ [] = []
movingLists n (x:xs)
    | length temp == n = temp : movingLists n xs
    | otherwise = []
    where
        temp = take n (x:xs)
