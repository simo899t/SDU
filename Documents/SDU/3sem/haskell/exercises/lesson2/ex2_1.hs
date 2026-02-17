bools :: [Bool]
bools = [True, True, False]

nums :: [[Int]]
nums = [[1,2,3],[4,5,6],[7,8,9]]

add :: Int -> Int -> Int -> Int
add firstInt secondInt thirdInt = firstInt + secondInt + thirdInt

copy :: a -> (a, a)
copy arg = (arg, arg)

apply :: (a -> b) -> a -> b
apply function arg = function arg