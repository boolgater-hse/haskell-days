{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Monad.Except
import Control.Applicative

data Excep a = Err String | Ok a
    deriving (Eq, Show)

(?/) :: (MonadError String m) => Double -> Double -> m Double
x ?/ 0 = throwError "Division by 0."
x ?/ y = return $ x / y

instance Functor Excep where
    fmap f (Ok a) = Ok (f a)
    fmap f (Err s) = Err s

instance Applicative Excep where
    pure a = Ok a
    (<*>) _ (Err s) = Err s
    (<*>) (Err s) _ = Err s
    (<*>) (Ok f) (Ok a) = Ok (f a)

instance Alternative Excep where
    empty = Err "Alternative.empty error."
    (<|>) (Err a) (Err b) = Err b
    (<|>) (Ok a) (Err s) = Err s
    (<|>) (Err s) (Ok a) = Err s
    (<|>) (Ok a) (Ok b) = Ok b

instance Monad Excep where
    return a = Ok a
    Ok a >>= f = f a
    Err s >>= _ = Err s

instance MonadFail Excep where
    fail s = Err "Monad.fail error."

instance (MonadError String) Excep where
    throwError = Err
    Err l `catchError` handler = handler l
    a `catchError` _ = a

instance MonadPlus Excep where
    mzero = Err "MonadPlus.mzero error."
    Err a `mplus` b = Err a
    a `mplus` b = b

example :: Double -> Double -> Excep String
example x y = action `catchError` return
    where
        action = do
        q <- x ?/ y
        guard (q >= 0)
        if q > 100 then do
            100 <- return q
            undefined
        else
            return (show q)
