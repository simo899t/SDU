import Data.Set

-- Problem 1: A decider for

phi :: Sentence Char
phi AllAre 'p' 'q'

-- Problem 2: The type of models
m0 :: Model Bool String
m0 = Mod (const empty)

m1 :: Model Integer Integer
m1 = Mod (\ z -> fromList [- abs z .. abs z])

-- Problem 2: A decider for 'entails'
