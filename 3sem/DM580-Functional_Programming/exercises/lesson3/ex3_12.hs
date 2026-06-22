toBinary :: Int -> [Int]
toBinary a
    | a < 0 = error "toBinary: negative input"
    | a == 0 = [0]
    | a > 0 = toBinary (a `div` 2) ++ [a `mod` 2]