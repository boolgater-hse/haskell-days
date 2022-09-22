import Data.Set (Set)
import qualified Data.Set as Set

digits :: Integer -> [Integer]
digits 0 = [0]
digits n = if (abs n `div` 10) == 0
    then
        [abs n `mod` 10]
    else
        digits (abs n `div` 10) ++ [abs n `mod` 10]

containsAllDigitsOnes :: Integer -> Bool
containsAllDigitsOnes n =
    let t = Set.toList (Set.fromList (filter (0 /=) (digits n))) in
        length t == 9 && length (filter (0 /=) (digits n)) == 9
