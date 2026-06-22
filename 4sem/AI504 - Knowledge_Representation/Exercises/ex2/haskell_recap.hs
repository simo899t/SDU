
{-
This function revesed list [a] and returns it.
for this with _ (hole) means that the return type is just [a] -> [a]

rev :: [a] -> [a]
rev = _ 
-}

{-
This function revesed list [a] and returns it.
on the otherhand, this should always return the type 

rev :: [a] -> [a]
rev [] = _     -- [a]
rev (x:xs) = _
-}

-------------------------------------------------

{-
This function revesed list [a] and returns it.
  curried    vs     uncrurried
A x B -> C        A -> (B -> C)
-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
rev :: [a] -> [a]
rev [] = []  
rev (x:xs) = app (rev xs) x     -- or use ++

app :: [a] -> a -> [a]
app [] x = [x]
app (y:ys) x = y: app ys x