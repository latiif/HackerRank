-- Duplicate Encoder

import Data.List
import Data.Char

duplicateEncode :: String -> String
duplicateEncode str' = map (\letter -> if letter `elem` duplicates then ')' else '(') str
    where 
        str = map toLower str'
        duplicates  = getDuplicates str



getDuplicates :: String -> String
getDuplicates str = map head $ filter (\str -> length str >1)  $ group $ sort $ map toLower str
