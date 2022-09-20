doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = if n < 0 then 0 else n * doubleFact (n - 2)

doubleFact' :: Integer -> Integer
doubleFact' 0 = 1
doubleFact' n = if n < 0 then 0 else product [n, n - 2..1]
