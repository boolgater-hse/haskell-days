foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f v xs = foldr (fun f) ini xs v

fun :: (t1 -> t2 -> t3) -> t2 -> (t3 -> t4) -> t1 -> t4
fun f elem g v = g (f v elem)

ini :: a -> a
ini = id
