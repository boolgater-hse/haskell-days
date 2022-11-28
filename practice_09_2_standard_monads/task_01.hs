{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Except

data ListIndexError =
    ErrTooLargeIndex Int
    | ErrNegativeIndex
    | OtherErr String
        deriving (Eq, Show)

infixl 9 !!!

(!!!) :: MonadError ListIndexError m => [a] -> Int -> m a
xs !!! n
    | n < 0 = throwError ErrNegativeIndex
    | enoughLen (n + 1) xs = return (xs !! n)
    | otherwise = throwError $ ErrTooLargeIndex n
        where
            enoughLen 0 _ = True
            enoughLen _ [] = False
            enoughLen n (_:ys) = enoughLen (n - 1) ys
