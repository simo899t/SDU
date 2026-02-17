data Tree a
    = Leaf a
    | Node (Tree a) a (Tree a)
    deriving Show

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node left x right) = Node (fmap g left) (g x) (fmap g right)