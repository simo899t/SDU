recognize :: String -> Bool
recognize str = length (filter (== '[') str) == length (filter (== ']') str)
main :: IO ()
main = return ()
