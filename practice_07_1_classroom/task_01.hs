doNTurns :: Int -> Board -> [Board]
doNTurns 0 ini = [ini]
doNTurns n ini = do
    a <- next ini
    doNTurns (n - 1) a

doNTurns' :: Int -> Board -> [Board]
doNTurns' 0 ini = [ini]
doNTurns' n ini = next ini >>= \a -> doNTurns (n - 1) a
