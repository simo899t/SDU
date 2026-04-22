halve :: [a] -> ([a], [a])
halve list = (take n list, drop n list)
  where n = length list `div` 2
main :: IO ()
main = return ()
