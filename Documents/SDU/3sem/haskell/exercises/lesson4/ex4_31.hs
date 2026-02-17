data MyMaybe a = MyNothing | MyJust a
instance Eq a => Eq (MyMaybe a) where
    MyNothing == MyNothing = True
    MyJust x == MyJust y = x == y
    _ == _ = False