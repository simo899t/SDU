-- Exercise 1
(<+) :: Maybe a -> Maybe a -> Maybe a
Nothing <+ Just y = Just y
x       <+_ = x

