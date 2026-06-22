recognizePal :: String -> Bool
recognizePal [] = True  -- Empty string is palindrome
recognizePal [_] = True  -- Single character is palindrome
recognizePal (x:xs) 
    | x == last xs = recognizePal (init xs)  -- If first == last, remove both and recurse
    | otherwise = False  -- not a palindrome$