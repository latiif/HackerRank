import Data.List
import Data.Char

camelCase :: String -> String
camelCase str = concat 
                $ map (\w-> toUpper (head w):tail w)  
                $ words 
                $ map toLower str

