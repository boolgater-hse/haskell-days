seqB :: Integer -> Integer
seqB 0 = 1
seqB 1 = 2
seqB 2 = 3
seqB n = helper 1 2 3 (n - 2)

helper :: (Eq t1, Num t1, Num t2) => t2 -> t2 -> t2 -> t1 -> t2
helper a b c 0 = c
helper a b c n = helper b c (c - (2 * b) + (3 * a)) (n - 1)
