infixl 9 !!!

(!!!) :: [a] -> Int -> Maybe a
xs !!! n = foldr fun ini xs n

fun :: (Ord b, Num b) => a -> (b -> Maybe a) -> b -> Maybe a
fun elem acc n
    | n == 0 = Just elem
    | n < 0 = Nothing
    | otherwise = acc (n - 1)

ini :: b -> Maybe a
ini n = Nothing
