import Control.Monad.Writer

minusLoggedR :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedR ini [] = do
    tell (show ini)
    return ini
minusLoggedR ini (x:xs) = do
    tell ("(" ++ show x ++ "-")
    result <- minusLoggedR ini xs
    tell ")"
    return (x - result)
