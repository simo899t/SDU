(^.) :: Int -> Int -> Int
a^.0 = 0
a^.b = a * (a^.(b - 1))
