{-# LANGUAGE RecordWildCards #-}

data Result a
    = Err [String]
    | Result a
    deriving Show

instance Functor Result where
    fmap :: (a -> b) -> Result a -> Result b
    fmap g (Err x) = Err x
    fmap g (Result x) = Result (g x)

instance Applicative Result where
    pure :: a -> Result a
    pure = Result
    (<*>) :: Result (a -> b) -> Result a -> Result b
    (Err f)     <*> (Err x)     = Err (f ++ x)
    (Err f)     <*> (Result x)  = Err f
    (Result f)  <*> (Err x)     = Err x
    (Result f)  <*> (Result x)  = Result (f x)

    
checkName :: String -> Result String
checkName _ = pure "name"

checkEmail :: String -> Result String
checkEmail _ = pure "email"

checkAge :: Int -> Result Int
checkAge = pure

data User = User
    {   name    ::  String
    ,   email   ::  String
    ,   age     ::  Int }

validate :: User -> Result User
validate User{..} = User
    <$> checkName name
    <*> checkEmail email
    <*> checkAge age
    where
        checkName = undefined
        checkEmail = undefined
        checkAge = undefined
main = do
    u <- validate <$> (User <$> (putStrLn "Enter name: " >> getLine)
                            <*> (putStrLn "Enter email: " >> getLine)
                            <*> (putStrLn "Enter age: " >> readLn ))
    case u of
        Err es -> do
            putStrLn $ "Validation failed. Error(s):"
                        ++ concat (("\n- " ++) <$> es)
                        ++ "\n"
            putStrLn "Please start over."
            main
        Result x -> putStrLn "Validation succeeded!\n"