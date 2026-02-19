module Test where


x :: Int





x = 1

-- Example of using different commands
num9 :: Int
num9 = 9

sqrtNum9 :: Double
sqrtNum9 = sqrt (fromIntegral num9)

piVal :: Double
piVal = pi

ePowNum9 :: Double
ePowNum9 = exp (fromIntegral num9)

logNum9 :: Double
logNum9 = log (fromIntegral num9)

sqaredNum9 :: Double
sqaredNum9 = (fromIntegral num9 :: Double) ** 2

truncvall :: Integer
truncvall = truncate 9.999

roundVal :: Integer
roundVal = round 9.999

ceilingVal :: Integer
ceilingVal = ceiling 9.999

floorVal :: Integer
floorVal = floor 9.999

trueOrFalse :: Bool
trueOrFalse = True

-- combine with lists
firstList :: [Integer]
firstList = 2 : [1,2,3] -- [2,1,2,3] (makes new list ofc)

-- or just concatenate lists
secondList :: [Integer]
secondList = [4,5,6]
combinedList :: [Integer]
combinedList = firstList ++ secondList -- [2,1,2,3,4,5,6]  (note that haskell uses simply linked lists) 


-- list comprehension
listComprehension :: [Double]
listComprehension = [x ** 2 | x <- [1..10], x <= 5] -- square of numbers from 1 to 10, but only those <= 5

listComprehension2 = [x | x <- [1..100], x `mod` 13 == 0] -- square of numbers from 1 to 10, but only those <= 5

-- infinite list example (only executes when needed)
infList = [0,1..]
infList2 = [x ** 2 | x <- takeWhile (<= 100) infList] -- infinite list of even numbers

-- main function to display results
main :: IO ()
main = do
    putStrLn $ "x: " ++ show x
    putStrLn $ "Square root of 9: " ++ show sqrtNum9
    putStrLn $ "Value of pi: " ++ show      piVal
    putStrLn $ "e raised to the power of 9: " ++ show ePowNum9
    putStrLn $ "Logarithm of 9: " ++ show logNum9
    putStrLn $ "9 squared: " ++ show sqaredNum9
    putStrLn $ "Truncated value of 9.999: " ++ show truncvall
    putStrLn $ "Rounded value of 9.999: " ++ show roundVal
    putStrLn $ "Ceiling value of 9.999: " ++ show ceilingVal
    putStrLn $ "Floor value of 9.999: " ++ show floorVal
    putStrLn $ "True or False (True && False): " ++ show trueOrFalse
    


-- this is just how to set up file for testing in ghci
{-
include "module <FileName> where" in top of file
save this file as <FileName>.hs

in terminal (either in vs or OS terminal):
path> ghci
ghci> :l FileName.hs 
[1 of 1] Compiling Test             ( Test.hs, interpreted )
Ok, one module loaded.

your're good to go
-}