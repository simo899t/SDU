anyTrue :: [Bool] -> Bool
anyTrue [] = False
anyTrue (x:xs)
    | not x = anyTrue xs
    | otherwise = False
main :: IO ()
main = return ()
