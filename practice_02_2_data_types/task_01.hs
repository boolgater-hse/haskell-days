data LogLevel = Info | Warning | Error deriving (Enum)

cmp :: LogLevel -> LogLevel -> Ordering
cmp a b
    | a1 > b1 = GT
    | a1 < b1 = LT
    | otherwise = EQ
    where
        a1 = fromEnum a
        b1 = fromEnum b
