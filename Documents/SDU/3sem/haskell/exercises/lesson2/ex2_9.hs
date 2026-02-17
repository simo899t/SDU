subList :: Eq a => [a] -> [a] -> Bool
subList [] ys = True
subList (x:xs) ys = go x ys
    where
        go x ys' = case ys of
            [] -> False
            (y:ys') | x == y -> subList xs ys'
                    | otherwise -> go x ys'