rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs
    | null (drop n xs) = helper (mod n (length xs)) xs
    | otherwise = helper n xs
    where
        helper :: Int -> [a] -> [a]
        helper _ [] = []
        helper 0 xs = xs
        helper n xs
            | n > 0 = drop n xs ++ take n xs
            | n < 0 = helper (length xs - (-n)) xs
            | otherwise = helper n xs
