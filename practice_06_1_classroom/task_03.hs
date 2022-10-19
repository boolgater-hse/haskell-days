import Control.Applicative
import Lib

nat :: Parser Char Int
nat = fst <$> nat'

nat' :: Parser Char (Int, Int)
nat'=
    (\x (y, nums) -> (x * (10 ^ nums) + y, nums + 1)) <$> digit
    <*> (nat' <|> pure (0, 0))
