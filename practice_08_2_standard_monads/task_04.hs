import Data.IORef

while :: IORef a -> (a -> Bool) -> IO () -> IO ()
while ref p action = do
    clear <- readIORef ref
    let ret | p clear = action >> while ref p action
            | otherwise = return ()
    ret
