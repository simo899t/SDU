luhnDouble :: Int -> Int
luhnDouble x = if 2 * x > 9 then 2 * x - 9
               else 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn first second third fourth = 
    ((luhnDouble first
      + luhnDouble second
      + luhnDouble third 
      + luhnDouble fourth) `mod` 10) == 0
    