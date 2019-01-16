import Data.List

aggregate :: Int -> [a] -> [[a]]
aggregate n xs = transpose $ aggregate' n xs

aggregate' :: Int -> [a] -> [[a]]
aggregate' _ [] = []
aggregate' 1 x = [x]
aggregate' n xs = xs:(aggregate' (n-1) (tail xs))


findLongestWord :: [[String]] -> String
findLongestWord possibilites = longest $ map concat possibilites


longest :: [String] -> String
longest xss = snd $ findLongest ([(length xs, xs) | xs <- xss]) (0,"")
    where 
        findLongest :: [(Int,String)] -> (Int,String) -> (Int,String)
        findLongest [] s = s
        findLongest ((l,w):xs) (maxL,maxW) | l > maxL = findLongest xs (l,w)
                                           | otherwise = findLongest xs (maxL,maxW)



longestConsec :: [String] -> Int -> String
longestConsec [] _ = ""
longestConsec strarr k  
                | k > n  = ""
                | k <= 0 = ""
                | otherwise = findLongestWord $ aggregate k strarr
            where n = length strarr