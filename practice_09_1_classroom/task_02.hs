{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Identity (Identity(..), ap)
import Control.Monad.State

newtype StrRdrT m a = StrRdrT { runStrRdrT :: String -> m a }

instance Monad m => Monad (StrRdrT m) where
    return :: a -> StrRdrT m a
    return a = StrRdrT (\str -> return a)

    (>>=) :: StrRdrT m a -> (a -> StrRdrT m b) -> StrRdrT m b
    (>>=) (StrRdrT f) g = StrRdrT (\str ->
        let ma = f str
            mb = ma >>= \a -> runStrRdrT (g a) str
        in mb)

instance MonadFail m => MonadFail (StrRdrT m)  where
    fail :: String -> StrRdrT m a
    fail err = StrRdrT (\str -> fail err)

instance Monad m => Functor (StrRdrT m) where
    fmap f (StrRdrT g) = StrRdrT (fmap f . g)

instance Monad m => Applicative (StrRdrT m) where
    pure = return
    (<*>) = ap

askStrRdr :: Monad m => StrRdrT m String
askStrRdr = StrRdrT return

asksStrRdr :: Monad m => (String -> a) -> StrRdrT m a
asksStrRdr f = do
    f <$> askStrRdr

type StrRdr = StrRdrT Identity

runStrRdr :: StrRdr a -> String -> a
runStrRdr (StrRdrT f) str = a
    where
        Identity a = f str

stSrTst :: StateT Int StrRdr Int
stSrTst = do
    a <- get
    n <- lift $ asksStrRdr length
    modify (+n)
    return a

srtTst' :: StrRdr (String,Int)
srtTst' = do
    env <- askStrRdr
    len <- asksStrRdr length
    return (env,len)
