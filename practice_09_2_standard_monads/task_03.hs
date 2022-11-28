import Control.Monad.Except
import Data.Char
import Data.List (genericLength)

data ParseError = ParseError {location :: Int, reason :: String}

type ParseMonad = Either ParseError

parseDig :: Char -> Integer -> ParseMonad Integer
parseDig x pos = do
    let go | isHexDigit x = return (toInteger (digitToInt x))
           | otherwise = throwError (ParseError (fromInteger pos) (x : ": invalid digit"))
    go

parseHex' :: String -> Integer -> Integer -> ParseMonad Integer
parseHex' [] pos acc = undefined
parseHex' [x] pos acc = do
    b <- parseDig x pos
    return (acc + b)
parseHex' (x:xs) pos acc = do
    b <- parseDig x pos
    parseHex' xs (pos + 1) (acc + b * pow 16 (genericLength xs))

parseHex :: String -> ParseMonad Integer
parseHex [] = return 0
parseHex x = parseHex' x 1 0

pow :: Integer -> Integer -> Integer
pow n 0 = 1
pow n t = n * pow n (t - 1)

printError :: ParseError -> ParseMonad String
printError pe = Right $ "At pos " ++ show (location pe) ++ ": " ++ reason pe

test s = str
    where
        (Right str) = do
            n <- parseHex s
            return $ show n
            `catchError` printError
