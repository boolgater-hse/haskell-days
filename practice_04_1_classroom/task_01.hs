drop' :: Int -> [a] -> [a]
drop' n xs = foldr step ini xs n

step :: a -> (Int -> [a]) -> Int -> [a]
step x next n
    | n > 0 = next (n - 1)
    | otherwise = x : next n

ini :: Int -> [a]
ini a = []
