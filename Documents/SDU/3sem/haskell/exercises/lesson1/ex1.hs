-- 1. Product of a list of integers
productOfList :: [Int] -> Int
productOfList [x] = x
productOfList (x:xs) = x * productOfList xs

-- 2. Quick sort implementation
qSort :: [Int] -> [Int]
qSort [] = []
qSort (x:xs) = qSort smaller ++ [x] ++ qSort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x] 

-- 2. Another version of Quick sort implementation (but reversed order)
qSort2 :: [Int] -> [Int]
qSort2 [] = []
qSort2 (x:xs) = qSort2 larger ++ [x] ++ qSort2 smaller
  where
    larger = [a | a <- xs, a > x]
    smaller  = [b | b <- xs, b <= x] 

-- 3. it would skip steps for faster execution

-- 4. fixing errors
-- N = a `div' length xs
--     where
--         a = 10
--        xs = [1,2,3,4,5]

-- 4. fixed version
n :: Int
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]


-- 5. Get the last element of a list (without using the built-in last function)
lastElement :: [a] -> a
lastElement xs = xs !! (length xs - 1)

-- 6. isLeapYear function
isLeapYear :: Int -> Bool
isLeapYear year
    | year `mod` 400 == 0 = True
    | year `mod` 100 == 0 = False
    | year `mod` 4 == 0   = True
    | otherwise           = False

-- 7. Days/Hours/Minutes/Seconds
timeConvert :: (Int, Int, Int) -> (Int, Int, Int, Int)
timeConvert (days, hours, minutes) = (totalDays, totalHours, totalMinutes, totalSeconds)
  where
    totalDays = days
    totalHours = days * 24 + hours
    totalMinutes = totalHours * 60 + minutes
-- 8. Debugging Insertion Sort

-- isSorted
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- almostSort :: (Ord a) => [a] -> [a]
almostSort :: [Int] -> [Int]
almostSort [] = []
almostSort (x:y:xs) -- this is the bug, it should just call almostInsert
   | x > y = y : x : almostSort xs
almostSort (x:xs) = almostInsert x (almostSort xs)

almostInsert :: Int -> [Int] -> [Int]
almostInsert x [] = [x]
almostInsert x (y:ys)
   | x <= y = x : y : ys
   | otherwise = y : almostInsert x ys

-- ghci> isSorted (almostSort [3,2,1])
-- False
-- ghci> isSorted (almostSort [3,2,1])
-- False

