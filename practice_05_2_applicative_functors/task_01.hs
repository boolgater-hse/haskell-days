import Control.Applicative (ZipList(ZipList), getZipList)

x1s :: [Integer]
x2s :: [Integer]
x3s :: [Integer]
x4s :: [Integer]
[x1s,x2s,x3s,x4s] = [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]

(>$<) :: (a -> b) -> [a] -> [b]
(>$<) = (<$>)

(>*<) :: [a -> b] -> [a] -> [b]
(>*<) xs ys = getZipList (ZipList xs <*> ZipList ys)
