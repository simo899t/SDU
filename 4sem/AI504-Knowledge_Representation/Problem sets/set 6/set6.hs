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
data ProofTree p
    = Axiom p
    | Assume p p
    | Barbara (ProofTree p) (ProofTree p)

type Graph v = Set (v,v)

deriving instance (Eq p) => Eq (Sentence p)
deriving instance (Ord p) => Ord (Sentence p)

allGraph :: Ord p => Set (Sentence p) -> Main.Graph p
allGraph = S.map (\(AllAre p q) -> (p, q))

isPrependable :: Eq v => (v,v) -> NonEmpty v -> Bool
isPrependable (_, v) (w :| _) = v == w

prepend :: (v,v) -> NonEmpty v -> NonEmpty v
prepend (a,_) (w:| rest) = a :| (w : rest)

prependables :: Eq v => Set (v,v) -> Set (NonEmpty v) -> Set ((v,v), NonEmpty v)
prependables edges paths = S.filter (uncurry isPrependable) (S.cartesianProduct edges paths)

addPrependResults :: Ord v => Main.Graph v -> Set (NonEmpty v) -> Set (NonEmpty v)
addPrependResults g s = s `S.union` S.map (uncurry prepend) (prependables g s)

iterateN :: Int -> (a -> a) -> a -> a 
iterateN 0 _ b = b
iterateN n f z = f (iterateN (n-1) f z)

pathsTo :: Ord v => Main.Graph v -> v -> Set (NonEmpty v)
pathsTo g b = iterateN (S.size g) (addPrependResults g) (S.singleton (b:|[]))


-- Problem set 6
findPathFrom :: Ord v => v -> Set (NonEmpty v) -> Maybe (NonEmpty v)
findPathFrom a paths = S.lookupMin (S.filter (\(x :| _) -> x == a) paths)

deriving instance (Eq p) => Eq (ProofTree p)

pathToTree :: NonEmpty p -> ProofTree p
pathToTree (x :| []) = Axiom x
pathToTree (x :| (y:ys)) = Data.Foldable.foldl Barbara (Assume x y) (Prelude.zipWith Assume (y:ys) ys)


canMod :: Ord p => (p -> Set (NonEmpty p)) -> Model p p
canMod f = Mod (\u -> S.map NE.head (f u))


decide :: Ord p => Set (Sentence p) -> Sentence p -> Either (ProofTree p) (Model p p)
decide ga (AllAre p q) =
        let gr = allGraph ga
            tc = pathsTo gr q
        in  case findPathFrom p tc of
            Just path -> Left (pathToTree path)
            Nothing   -> Right (canMod (pathsTo gr))

-- TESTING

-- >>> findPathFrom 3 (S.fromList [1 :| [2, 3], 3 :| [2], 3 :| [1, 1]])
-- Just (3 :| [1,1])

-- >>> findPathFrom 9 (S.fromList [1 :| [2, 3], 3 :| [2], 3 :| [1, 1]])
-- Nothing


path :: NonEmpty Integer
path = 1:|[2,3,4]

tree :: ProofTree Integer
tree = Barbara 
        (Barbara 
            (Assume 1 2) (Assume 2 3)) 
            (Assume 3 4)

path2 :: NonEmpty Char
path2 = 'a' :| "bcde"
tree2 :: ProofTree Char
tree2 = Barbara
            (Barbara
              (Barbara (Assume 'a' 'b') (Assume 'b' 'c'))
              (Assume 'c' 'd'))
            (Assume 'd' 'e')


-- >>> pathToTree path == tree
-- True

-- >>> pathToTree path2 == tree2
-- True

f :: Int -> Set (NonEmpty Int)
f 2 = S.fromList [1 :| [2], 2 :| []]
f 3 = S.fromList [1 :| [2, 3], 2 :| [3], 3 :| []]
f z = S.singleton (z :| [])


-- >>> canMod f |= AllAre 1 3
-- True


-- >>> either (== Barbara (Assume 1 2) (Assume 2 3)) (const False) (decide (S.fromList [AllAre 1 2, AllAre 2 3]) (AllAre 1 3))
-- True

-- >>> either (const False) (\m -> m |= AllAre 1 2 && m |= AllAre 2 3 && not (m |= AllAre 2 1)) (decide (S.fromList [AllAre 1 2, AllAre 2 3]) (AllAre 2 1))
-- True
