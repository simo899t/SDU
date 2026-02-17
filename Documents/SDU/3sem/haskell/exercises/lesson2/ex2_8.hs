keepAfter :: Eq a => a -> [a] -> [a]
keepAfter _ [] = []
keepAfter x (y:ys)
 | x == y = ys
 | otherwise = keepAfter x ys