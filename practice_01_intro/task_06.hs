sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (mySum x, myCount x)

mySum :: Integer -> Integer
mySum 0 = 0
mySum x
    | x < 0 = mySum ((-1) * x)
    | otherwise = getLastDigit x + mySum (x `div` 10)

getLastDigit :: Integer -> Integer
getLastDigit x
    | x < 0 = ((-1) * x) `rem` 10
    | otherwise = x `rem` 10

myCount :: Integer -> Integer
myCount x
  | x < 0 = myCountHelper 0 (-x)
  | otherwise = myCountHelper 0 x

myCountHelper :: Integer -> Integer -> Integer
myCountHelper c x
  | x /= 0 = myCountHelper (c + 1) (x `div` 10)
  | c == 0 = c + 1
  | otherwise = c
