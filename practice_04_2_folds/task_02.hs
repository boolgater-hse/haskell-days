tails' :: [a] -> [[a]]
tails' = foldr fun ini

fun :: a -> [[a]] -> [[a]]
fun x y = (x : head y) : y

ini :: [[a]]
ini = [[]]

inits' :: [a] -> [[a]]
inits' = foldr fun' ini'

fun' :: a -> [[a]] -> [[a]]
fun' x y = [] : map (x :) y

ini' :: [[a]]
ini' = [[]]
