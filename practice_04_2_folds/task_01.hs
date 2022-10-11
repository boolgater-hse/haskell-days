import Data.List (unfoldr)
revRange :: (Char,Char) -> [Char]
revRange = unfoldr fun

fun :: (Char, Char) -> Maybe (Char, (Char, Char))
fun b
    | fromEnum (fst b) > fromEnum (snd b) = Nothing
    | fromEnum (snd b) == fromEnum (fst b) - 1 = Nothing
    | otherwise = Just (snd b, (fst b, toEnum (fromEnum (snd b) - 1) :: Char))
