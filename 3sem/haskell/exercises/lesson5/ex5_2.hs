adder :: IO ()
adder = do
    putStrLn "How many numbers?"
    line <- getLine
    let num = read line :: Int
    adderAcc 0 num

-- auxiliary function
adderAcc :: Int -> Int -> IO ()
adderAcc total 0 = print total
adderAcc total num = do
    line <- getLine
    let input = read line :: Int
    adderAcc (total+input) (num-1)


sliding :: IO ()
sliding = do
    let window = [] :: [Int]
    putStrLn ("Window: " ++ show window)
    putStrLn "Next number?"
    nxtNum <- getLine
    let num = read nxtNum :: Int
    slidingAcc window num

-- auxiliary function
slidingAcc :: [Int] -> Int -> IO()
slidingAcc window num = do
    let newWindow = if length window >= 3 
                    then tail window ++ [num]
                    else window ++ [num]
    putStrLn ("Window: " ++ show newWindow)
    putStrLn "Next number?"
    nxtNum <- getLine
    let nextNum = read nxtNum :: Int
    slidingAcc newWindow nextNum