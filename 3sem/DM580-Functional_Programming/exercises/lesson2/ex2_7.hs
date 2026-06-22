and1 :: Bool -> Bool -> Bool
True `and1` True = True
_ `and1` _ = False


and2 :: Bool -> Bool -> Bool
True `and2` b = b
False `and2` _ = False