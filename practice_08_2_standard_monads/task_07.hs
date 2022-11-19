import System.Random
import Data.List

average :: [Double] -> Double
average xs = sum xs / genericLength xs

absDeviation :: [Int] -> Double
absDeviation xs = abs ((genericLength xs / 2) - foldr (\a b -> if a == 1 then b + 1 else b) 0 xs)

avgdev'' :: Int -> Int -> Double
avgdev'' k n = average (helper k n)
    where
        helper 0 n = []
        helper k n = absDeviation (take n (randomRs (0, 1) (mkStdGen (777 + k * n + 228 * 2)))) : helper (k - 1) n
