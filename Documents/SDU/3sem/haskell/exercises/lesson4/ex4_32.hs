data Expr = Val Int | 
instance Show Expr where
    show (Val x) = show x
    show (Add x y) = show x ++ 