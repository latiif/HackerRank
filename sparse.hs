import Data.List

-- Sparse Arrays

-- Convert a list of strings into a dictionary
organize :: [String] -> [(String,Int)]
organize dic = map (\e -> (head e, length e)) $ (group . sort) dic

-- Finds the number of appearances for a given word
findInDic :: String  -> [(String,Int)] -> Int
findInDic entry dic = case lookup entry dic of
                    (Just x) -> x
                    _        -> 0

-- Finds the number of occurences for queries in strings                     
solve :: [String] -> [String] -> [Int]
solve strings queries = map (\query -> findInDic query dic) queries
    where dic = organize strings        
   
    
-- displays an array of ints in desired format    
displayAnswer :: [Int] -> String
displayAnswer ints = concat $ map (\int -> show (int)++"\n") ints    


-- reads n number of strings
readLines :: Int -> IO [String]
readLines 0 = return []
readLines n = do
        el <- getLine
        rest <- readLines (n-1) 
        return (el:rest)    

main :: IO ()
main = do
    nStrings <- getLine
    let n = (read nStrings):: Int
    strings <- readLines n

    nQueries <- getLine
    let q = (read nQueries) :: Int
    queries <- readLines q
    
    putStrLn (displayAnswer (solve strings queries))
