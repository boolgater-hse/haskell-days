swap :: (b, a) -> (a, b)
swap = uncurry (flip (,))
