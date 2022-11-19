import System.Random
import Data.List
import Control.Monad.State

average :: [Double] -> Double
average xs = sum xs / genericLength xs

flips :: RandomGen g => Int -> State g [Int]
flips 0 = return []
flips n = do
    x <- randomRState (0, 1)
    xs <- flips (n - 1)
    return (x:xs)

absDeviation :: [Int] -> Double
absDeviation xs = abs ((genericLength xs / 2) - foldr (\a b -> if a == 1 then b + 1 else b) 0 xs)

randomRState :: (Random a, RandomGen g) => (a, a) -> State g a
randomRState (x,y) = do state (randomR (x, y))

avgdev' :: Int -> Int -> State StdGen Double
avgdev' k n = do
    results <- helper k n
    return (average results)
        where
            helper 0 n = return []
            helper k n = do
                sample <- flips n
                let adev = absDeviation sample
                xs <- helper (k - 1) n
                return (adev:xs)
