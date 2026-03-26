import Data.Set

-- Problem 1: A decider for

data Sentence p = AllAre p p

-- Problem 2: The type of models

data Model p m = Mod (p -> Set m)

-- Problem 3: A decider for 'entails'

(|=) :: (Ord m) => Model p m -> Sentence p -> Bool
Mod f |= AllAre p q = f p `isSubsetOf` f q

m0 :: Model Bool String
m0 = Mod (const empty)

m1 :: Model Integer Integer
m1 = Mod (\z -> fromList [-abs z .. abs z])

-- >>> m1 |= AllAre (-3) 2
-- False

-- >>> m0 |= AllAre True False
-- True
