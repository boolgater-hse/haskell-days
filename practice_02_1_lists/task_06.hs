repeatEveryElem :: Int -> [a] -> [a]
repeatEveryElem n = concatMap (spam n)

spam :: Int -> a -> [a]
spam n x = if n == 0 then [] else spam (n - 1) x ++ [x]

-- Raw version
-- repeatEveryElem' :: Int -> [a] -> [a]
-- repeatEveryElem' n [] = []
-- repeatEveryElem' n (x:xs) = spam n x ++ repeatEveryElem' n xs
