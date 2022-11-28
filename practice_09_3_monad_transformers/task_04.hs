{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Functor.Identity
import Control.Monad.State

data Logged a = Logged String a
    deriving (Eq, Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

type Logg = LoggT Identity

instance Monad m => Functor (LoggT m) where
    fmap = liftM

instance Monad m => Applicative (LoggT m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (LoggT m) where
    return a = LoggT (return (Logged mempty a))
    (>>=) (LoggT a) b = LoggT $ do
        (Logged d a) <- a
        (Logged c b) <- runLoggT (b a)
        return (Logged (c `mappend` d) b)

instance MonadFail m => MonadFail (LoggT m) where
    fail = LoggT . fail

instance MonadTrans LoggT where
    lift m = LoggT $ do
        Logged mempty <$> m

instance MonadState s m => MonadState s (LoggT m) where
    get = lift get
    put s = lift (put s)
    state f = lift (state f)

write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT (return (Logged s ()))

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

logSt' :: LoggT (State Integer) Integer
logSt' = do
    modify (+1)                   -- no lift!
    a <- get                      -- no lift!
    write2log $ show $ a * 10
    put 42                        -- no lift!
    return $ a * 100
