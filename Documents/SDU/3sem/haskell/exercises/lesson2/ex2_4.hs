{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use splitAt" #-}

halve :: [a] -> ([a], [a])
halve list = (take n list, drop n list)
  where n = length list `div` 2