data Expr = Val Int | Add Expr Expr

instance Show Expr where
    show (Val x)   = show x
    show (Add x y) = show x ++ " + " ++ show y

main :: IO ()
main = return ()
