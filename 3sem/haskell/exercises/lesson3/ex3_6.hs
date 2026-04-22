scalarprod :: [Int] -> [Int] -> Int
scalarprod a b = sum [x * y | (x, y) <- zip a b]