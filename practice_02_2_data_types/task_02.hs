data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

abbrFirstName :: Person -> Person
abbrFirstName p
    | length (firstName p) > 2 = p { firstName = take 1 (firstName p) ++ "." }
    | otherwise = p
