import Data.Array.IArray (listArray)
safetail1 :: [a] -> Maybe [a]
safetail1 list = if null list then Nothing
                 else Just (tail list)

safetail2 :: [a] -> Maybe [a]
safetail2 list
  | null list = Nothing
  | otherwise = Just (tail list)

safetail3 :: [a] -> Maybe [a]
safetail3 (_:xs) = Just xs
safetail3 [] = Nothing
