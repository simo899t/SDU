grid :: Int -> Int -> [(Int, Int)]
grid a b = [(x,y) | y <- [1..a], x <- [1..b]]