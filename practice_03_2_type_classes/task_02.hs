import Data.Complex

newtype Cmplx = Cmplx (Complex Double) deriving Eq

instance Show Cmplx where
    show (Cmplx eq) = show real ++ (if img < 0 then "-i*" else "+i*") ++ (if img < 0 then show (-img) else show img)
        where
            real = realPart eq
            img = imagPart eq

instance Read Cmplx where
    readsPrec _ input = parseComplex input
        where
            parseComplex :: String -> [(Cmplx, String)]
            parseComplex input
                | isPatternInString input "+i*" = [(Cmplx ((read (allBeforePattern input "+i*") :: Double) :+ (read (allAfterPattern input "+i*") :: Double)), "")]
                | otherwise = [(Cmplx ((read (allBeforePattern input "-i*") :: Double) :+ (-1) * (read (allAfterPattern input "-i*") :: Double)), "")]

allBeforePattern :: String -> String -> String
allBeforePattern str [] = str
allBeforePattern [] _ = []
allBeforePattern str@(x : xs) patt
    | take (length patt) str == patt = ""
    | otherwise = x : allBeforePattern xs patt

allAfterPattern :: String -> String -> String
allAfterPattern str [] = str
allAfterPattern [] _ = []
allAfterPattern str@(x : xs) patt
    | take (length patt) str == patt = reverse (take (length str - length patt) (reverse str))
    | otherwise = allAfterPattern xs patt

isPatternInString :: String -> String -> Bool
isPatternInString str [] = False
isPatternInString [] _ = False
isPatternInString str@(x : xs) patt
    | take (length patt) str == patt = True
    | otherwise = isPatternInString xs patt

-- allBeforePattern "-2.7+i*3.4" "+i*" == "-2.7"
-- allBeforePattern "-2.7+i*3.4" "-i*" == "-2.7+i*3.4"

-- allAfterPattern "-2.7+i*3.4" "+i*" == 3.4
-- allAfterPattern "-2.7+i*3.4" "-i*" == ""
