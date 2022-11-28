import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (msum)
import Data.Char (isNumber, isPunctuation)

newtype PwdError = PwdError String

type PwdErrorIOMonad = ExceptT PwdError IO

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."

instance Semigroup PwdError where
    (<>) (PwdError a) (PwdError b) = PwdError ""

instance Monoid PwdError where
    mempty = PwdError ""
    mappend (PwdError a) (PwdError b) = PwdError ""

getValidPassword :: PwdErrorIOMonad String
getValidPassword = action `catchE` handler
    where
        action = do
            s <- liftIO getLine
            let go | length s < 8 = throwE (PwdError "Incorrect input: password is too short!")
                   | not (any isNumber s) = throwE (PwdError "Incorrect input: password must contain some digits!")
                   | not (any isPunctuation s) = throwE (PwdError "Incorrect input: password must contain some punctuations!")
                   | otherwise = return s
            go

        handler (PwdError e) = do
            liftIO (putStrLn e)
            throwE (PwdError e)
