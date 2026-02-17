listComp :: (a -> b) -> (a -> Bool) -> [a] -> [b]
listComp f p xs = map f (filter p xs)