{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Reader

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

instance MonadReader r m => MonadReader r (LoggT m) where
    ask = lift ask
    local = mapLoggT . local
    reader = lift . reader

write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT (return (Logged s ()))

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f = LoggT . f . runLoggT

class Monad m => MonadLogg m where
    w2log :: String -> m ()
    logg :: Logged a -> m a

instance Monad m => MonadLogg (LoggT m) where
    w2log = write2log
    logg = LoggT . return

instance MonadLogg m => MonadLogg (StateT s m) where
    w2log = lift . w2log
    logg  = lift . logg

instance MonadLogg m => MonadLogg (ReaderT r m) where
    w2log = lift . w2log
    logg  = lift . logg

logSt'' :: LoggT (State Integer) Integer
logSt'' = do
    x <- logg $ Logged "BEGIN" 1
    modify (+x)
    a <- get
    w2log $ show $ a * 10
    put 42
    w2log "END"
    return $ a * 100

rdrStLog :: ReaderT Integer (StateT Integer Logg) Integer
rdrStLog = do
    x <- logg $ Logged "BEGIN" 1
    y <- ask
    modify (+ (x+y))
    a <- get
    w2log $ show $ a * 10
    put 42
    w2log "END"
    return $ a * 100
