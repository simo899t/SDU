mySum :: Num a => a -> a -> a -> a
mySum firstNum secondNum thirdNum = firstNum + secondNum + thirdNum

myAvg :: Fractional a => a -> a -> a
myAvg first second = (first + second)/2

myRead :: Read a => String -> Maybe a
myRead val = case reads val of
    [(val, "")] -> Just val
    _           -> Nothing

testMyRead :: Bool
testMyRead = myRead "True" == Just True
             && myRead "False" == Just False
             && myRead "3.14159" == Just 3.14159