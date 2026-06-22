
import Data.Bits

-- Define a Register that holds an Int
newtype Register = Register Int deriving (Show, Eq) -- now both printable and comparable

-- Function to create a Register with a given value
storeVal :: Int -> Register -> Register
storeVal val _ = Register val

-- Function to load the value from a Register
loadVal :: Register -> Int
loadVal (Register val) = val

-- Add a number to the register
addToReg :: Register -> Int -> Register
addToReg (Register val) num = Register (val + num)

-- Function to increment the value in a Register
incReg :: Register -> Register
incReg (Register val) = addToReg (Register val) 1

-- Function to decrement the value in a Register
clearReg :: Register -> Register
clearReg _ = Register 0

-- Subtract a number from the register
subFromReg :: Register -> Int -> Register
subFromReg (Register val) num = Register (val - num)

-- Functionen to decrement the value in a Register
decReg :: Register -> Register
decReg (Register val) = subFromReg (Register val) 1

-- Multiply the value in the register by a number
mulReg :: Register -> Int -> Register
mulReg (Register val) num = Register (val * num)

-- Divide the value in the register by a number
divReg :: Register -> Int -> Register
divReg (Register val) num
  | num == 0 = error "Division by zero"
  | otherwise = Register (val `div` num)

safeDivReg :: Register -> Int -> Maybe Register
safeDivReg (Register val) num
  | num == 0  = Nothing
  | otherwise = Just (Register (val `div` num))

-- Modulo operation on the value in the register
modReg :: Register -> Int -> Register
modReg (Register val) num
  | num == 0 = error "Division by zero"
  | otherwise = Register (val `mod` num)

safeModReg :: Register -> Int -> Maybe Register
safeModReg (Register val) num
  | num == 0  = Nothing
  | otherwise = Just (Register (val `mod` num))

-- Bitwise operations on the value in the register

-- BitShiftLeft
bitShiftLeft :: Register -> Int -> Register
bitShiftLeft (Register val) bits = Register (val `shiftL` bits)

-- BitShiftRight
bitShiftRight :: Register -> Int -> Register
bitShiftRight (Register val) bits = Register (val `shiftR` bits)

-- Bitwise AND operation
andReg :: Register -> Register -> Register
andReg (Register val1) (Register val2) = Register (val1 .&. val2)

-- Bitwise OR operation
orReg :: Register -> Register -> Register
orReg (Register val1) (Register val2) = Register (val1 .|. val2)

-- Bitwise XOR operation
xorReg :: Register -> Register -> Register
xorReg (Register val1) (Register val2) = Register (val1 `xor` val2)

-- Bitwise NOT (complement) operation
notReg :: Register -> Register
notReg (Register val) = Register (complement val)

-- Set a specific bit (0-indexed)
setBit :: Register -> Int -> Register
setBit (Register val) pos = Register (Data.Bits.setBit val pos)

-- Clear a specific bit
clearBit :: Register -> Int -> Register
clearBit (Register val) pos = Register (Data.Bits.clearBit val pos)

-- print the register value
printRegister :: Register -> IO ()
printRegister (Register val) = putStrLn $ "Register value: " ++ show val

-- log changes to the register
logRegisterChange :: Register -> String -> IO ()
logRegisterChange (Register val) action = do
    putStrLn $ "Action: " ++ action ++ ", New Register value: " ++ show val

testSequence :: IO ()
testSequence = do
  let r0 = Register 5
  let r1 = addToReg r0 3
  let r2 = mulReg r1 2
  let r3 = decReg r2
  printRegister r3
  logRegisterChange r3 "After math ops"