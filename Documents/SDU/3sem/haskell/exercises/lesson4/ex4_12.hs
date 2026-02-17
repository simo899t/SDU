-- 1. decide if all elements of a list satisfy a predicate.
allB :: (a -> Bool) -> [a] -> Bool
allB p a = foldr ((&&) . p) True a

-- 2. Decide if any element of a list satisﬁes a predicate.
anyB :: (a -> Bool) -> [a] -> Bool
anyB p a = foldr ((||) . p) False a


-- you dont have to include a, since haskell knowssss

-- 3. Select elements from a list while they satisfy a predicate.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

-- Remove elements from a list while they satisfy a predicate:
dropWhile' :: (a -> Bool) -> [a] -> [a]