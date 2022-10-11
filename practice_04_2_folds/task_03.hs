reverse' :: [a] -> [a]
reverse' = foldr fun' ini'

fun' :: a -> [a] -> [a]
fun' x xs' = xs' ++ [x]

ini' :: [a]
ini' = []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

fun'' :: [a] -> a -> [a]
fun'' = flip (:)

ini'' :: [a]
ini'' = []
