import Data.Set (Set)
import qualified Data.Set as Set

digits :: Integer -> [Integer]
digits 0 = [0]
digits n = if (abs n `div` 10) == 0
    then
        [abs n `mod` 10]
    else
        digits (abs n `div` 10) ++ [abs n `mod` 10]

containsAllDigits :: Integer -> Bool
containsAllDigits n = length (filter (0 /=) (Set.toList (Set.fromList (digits n)))) == 9
