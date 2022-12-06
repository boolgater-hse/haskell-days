import RT.Lib

instance Functor B where
    fmap _ Empty = Empty
    fmap f (Zero b) = Zero (f b)
    fmap f (One b) = One (f b)

data B a = Empty | Zero a | One a
    deriving (Eq, Show)

type Bin = Fix B

phiB :: B Int -> Int
phiB Empty = 0
phiB (Zero x) = 2 * x
phiB (One x) = 2 * x + 1

bin2int :: Bin -> Int
bin2int = cata phiB

psiB :: Int -> B Int
psiB 0 = Empty
psiB x
    | even x = Zero (x `div` 2)
    | otherwise = One (x `div` 2)

int2bin :: Int -> Bin
int2bin = ana psiB
