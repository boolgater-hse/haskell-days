{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Writer

minusLoggedL :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedL ini ls = minusLoggedL' ini (reverse ls)
    where
        minusLoggedL' ini [] = do
            tell (show ini)
            return ini
        minusLoggedL' ini (x:xs) = do
            tell "("
            result <- minusLoggedL' ini xs
            tell ("-" ++ show x)
            tell ")"
            return (result - x)
