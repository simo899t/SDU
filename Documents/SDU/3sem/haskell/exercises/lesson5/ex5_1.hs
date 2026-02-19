import Data.List (intercalate)

data Book = Book
    {   title  :: String
    ,   author :: String
    ,   year   :: Int
    ,   loaned :: Bool
    } deriving Show

checkout :: Book -> Book
checkout b = b { loaned = not (loaned b) }

groupByAuthor :: [Book] -> [(String, [Book])]
groupByAuthor [] = []
groupByAuthor (x:xs) = (author x, x : [b | b <- xs, author b == author x]) : groupByAuthor [b | b <- xs, author b /= author x]

prettyLib :: [(String, [Book])] -> String
prettyLib xs = "[ "
    ++ intercalate "\n, " (map (prettyPair 1) xs)
    ++ " ]"
  where
    prettyPair :: Int -> (String, [Book]) -> String
    prettyPair i (auth, bs) =
        "(" ++ show auth ++ ",\n"
        ++ indent (i+1) ++ "[ "
        ++ intercalate (indent (i+1) ++ ", ") (map (prettyBook (i+2)) bs)
        ++ indent (i+1) ++ "])"
    prettyBook :: Int -> Book -> String
    prettyBook i b = "Book"
        ++ " { title = " ++ show (title b) ++ "\n"
        ++ indent i ++ " , author = " ++ show (author b) ++ "\n"
        ++ indent i ++ " , year = " ++ show (year b) ++ "\n"
        ++ indent i ++ " , loaned = " ++ show (loaned b) ++ " }\n"
    indent :: Int -> String
    indent n = replicate (n * 2) ' '