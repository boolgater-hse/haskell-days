import Control.Monad

surround :: a -> a -> [a] -> [a]
surround x y [] = []
surround x y (z:zs) = do
    (x : z : [y]) `mplus` surround x y zs
