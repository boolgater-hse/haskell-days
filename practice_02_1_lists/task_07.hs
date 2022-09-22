-- Infinite counting list
nats :: [Integer]
nats = 1 : map (+1) nats

movingLists :: Int -> [a] -> [[a]]
movingLists _ [] = []
movingLists n (x:xs)
    | length (take n (x:xs)) == n = take n (x:xs) : movingLists n xs
    | otherwise = []
