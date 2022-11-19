import System.Random
import Data.List

average :: [Double] -> Double
average xs = sum xs / genericLength xs

flips :: Int -> IO [Int]
flips 0 = return []
flips n = do
    x <- getStdRandom (randomR (0, 1))
    xs <- flips (n - 1)
    return (x:xs)

absDeviation :: [Int] -> Double
absDeviation xs = abs ((genericLength xs / 2) - foldr (\a b -> if a == 1 then b + 1 else b) 0 xs)

avgdev :: Int -> Int -> IO Double
avgdev k n = do
    results <- helper k n
    return (average results)
        where
            helper 0 n = return []
            helper k n = do
                sample <- flips n
                let adev = absDeviation sample
                xs <- helper (k - 1) n
                return (adev:xs)
