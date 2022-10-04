import Data.Foldable

data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Show a => Show (Tree a) where
    show Leaf = "{}"
    show (Node left num right) = "<" ++ show left ++ show num ++ show right ++ ">"

instance Read a => Read (Tree a) where
    readsPrec _ input = parseTree input
        where
            parseTree :: Read a => String -> [(Tree a, String)]
            parseTree ('{' : '}' : rest) = [(Leaf, rest)]
            parseTree ('<' : rest)       =
              foldMap
                (\(left , rest') ->
                  foldMap
                    (\(x, rest'') ->
                      foldMap
                        (\(right, rest''') -> case rest''' of
                          ('>': rest'''') -> [(Node left x right, rest'''')]
                          _               -> []
                        )
                        (parseTree rest'')
                    )
                    (reads rest')
                )
                (parseTree rest)
            parseTree _ = []
