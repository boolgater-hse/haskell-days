import Control.Monad.Identity
import Control.Monad.Cont
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

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT (return (Logged s ()))

logSt :: LoggT (State Integer) Integer
logSt = do
    lift $ modify (+1)
    a <- lift get
    write2log $ show $ a * 10
    lift $ put 42
    return $ a * 100
