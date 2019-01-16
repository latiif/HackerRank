
import Data.Char
import Data.List

isPangram :: String -> Bool
isPangram str = 26 == (length $ nub $ filter (`elem` alphabet) $ map toLower str)
    where alphabet = ['a'..'z']