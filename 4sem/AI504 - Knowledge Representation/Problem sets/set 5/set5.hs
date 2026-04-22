-- Imports
import Data.Foldable
import Data.List.NonEmpty as NE
import Data.Set as S
import Data.Graph (Graph)
import qualified Control.Applicative as Set

-- Problem set 3
data Sentence p = AllAre p p
data Model p m = Mod (p -> Set m)
(|=) :: (Ord m) => Model p m -> Sentence p -> Bool
Mod f |= AllAre p q = f p `isSubsetOf` f q

-- Problem set 5

-- PROBLEM 1: THE TYPE OF PROOF TREES
data ProofTree p
    = Axiom p
    | Assume p p
    | Barbara (ProofTree p) (ProofTree p)

-- PROBLEM 2: THE TYPE (ALIAS) OF GRAPHS
type Graph v = Set (v,v)

-- PROBLEM 3: THE ALL-GRAPH OF A SET OF FORMULAS
deriving instance (Eq p) => Eq (Sentence p)
deriving instance (Ord p) => Ord (Sentence p)

allGraph :: Ord p => Set (Sentence p) -> Main.Graph p
allGraph = S.map (\(AllAre p q) -> (p, q))

-- PROBLEM 4: REFLEXIVE-TRANSITIVE CLOSURE


-- 4.1
isPrependable :: Eq v => ((v,v), NonEmpty v) -> Bool
isPrependable ((_, v), w :| _) = v == w

-- 4.2
prepend :: ((v,v), NonEmpty v) -> NonEmpty v
prepend ((a,_), w:| rest) = a :| (w : rest)

-- 4.3
prependables :: Ord v => Set (v,v) -> Set (NonEmpty v) -> Set ((v,v), NonEmpty v)
prependables edges paths = foldMap (\edge -> concIf edge paths) edges

{-
result = []
for edge in edges:
    result += concIf(edge, paths)
return result
-}

concIf :: Ord v => (v,v) -> Set (NonEmpty v) -> Set ((v,v), NonEmpty v)
concIf edge paths = S.map (\path -> (edge, path)) (S.filter (\path -> isPrependable (edge, path)) paths)

{- 
kept = []
for path in paths:
    if isPrependable(edge, path):
        kept += path

result = []
for path in kept:
    result += [(edge, paths)]
return result
-}

-- 4.4
addPrependResults :: Ord v => Main.Graph v -> Set (NonEmpty v) -> Set (NonEmpty v)
addPrependResults g s = s `S.union` S.map prepend (prependables g s)

-- 4.5
iterateN :: Int -> (a -> a) -> a -> a 
iterateN 0 _ b = b
iterateN n f z = f (iterateN (n-1) f z)






-- TESTING

d :: ProofTree Char
d = Barbara
        (Barbara
            (Assume 'a' 'b')
            (Axiom 'b'))
        (Assume 'b' 'c')


g :: Main.Graph Char
g = S.fromList [
        ('w', 'x'),
        ('w', 'y'),
        ('y', 'w'),
        ('y', 'y')
    ]

-- >>> allGraph (S.fromList [AllAre 'w' 'x', AllAre 'w' 'y', AllAre 'y' 'w', AllAre 'y' 'y']) == g
-- True

-- >>> prependables (S.fromList [(0,1), (2,2)]) (S.fromList [1:|[3,5], 5:|[4,5], 1:|[4], 2:|[9]])
-- fromList [((0,1),1 :| [3,5]),((0,1),1 :| [4]),((2,2),2 :| [9])]

-- >>> addPrependResults g (S.fromList ['y':|"wx"])
-- fromList ['w' :| "ywx",'y' :| "wx",'y' :| "ywx"]

-- >>> iterateN 3 (2 *) 1
-- 8