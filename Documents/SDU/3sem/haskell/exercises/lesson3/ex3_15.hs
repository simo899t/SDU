percentageOf7s :: Int -> Double 
percentageOf7s a
  | a <= 0    = 0
  | otherwise = fromIntegral (length [x | x <- [1..a], digitInNumber 7 x]) / fromIntegral a

digitInNumber :: Integral a => a -> a -> Bool
digitInNumber d m
  | m == 0         = False
  | m `mod` 10 == d = True
  | otherwise      = digitInNumber d (m `div` 10)