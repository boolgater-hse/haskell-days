fibonacci :: Integer -> Integer
fibonacci (-1) = 1
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n =
    if n < 0
        then helper 1 1 (-n) * round ((-1)^^(n + 1))
        else helper 1 1 n

helper :: (Eq t1, Num t1, Num t2) => t2 -> t2 -> t1 -> t2
helper a b 2 = b
helper a b n = helper b (a + b) (n - 1)
