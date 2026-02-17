{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

third1 :: [a] -> a
third1 list = head (tail (tail list))

third2 :: [a] -> a
third2 list = list !! 3


third3 :: [a] -> Maybe a
third3 (_:_:x:_) = Just x
third3 _         = Nothing