


pascalsTriangle :: Int -> [Int]
pascalsTriangle n = concat $ [pascalsTriangle' i | i<-[1..n]]

pascalsTriangle' :: Int -> [Int]
pascalsTriangle' 1 = [1]
pascalsTriangle' x = [1]++(sumPairs $ getTwos $ pascalsTriangle' (x-1))++[1]



getTwos :: [Int] -> [(Int,Int)]
getTwos xs = zip xs (tail xs)

sumPairs :: [(Int,Int)] -> [Int]
sumPairs xs = map (\(a,b) -> a+b) xs