import Control.Monad
import Control.Monad.Identity

data Logged a = Logged String a
    deriving (Eq, Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

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

logTst :: LoggT Identity Integer
logTst = do
    x <- LoggT $ Identity $ Logged "AAA" 30
    y <- return 10
    z <- LoggT $ Identity $ Logged "BBB" 2
    return $ x + y + z

failTst :: [Integer] -> LoggT [] Integer
failTst xs = do
    5 <- LoggT $ fmap (Logged "") xs
    LoggT [Logged "A" ()]
    return 42
