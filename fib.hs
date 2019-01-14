fib :: [Integer] -> [Integer]
fib  nums = map (\(num,fibn) -> fibn `mod` (100000000+7)) 
    $ filter (\(num,fibn) -> num `elem` nums) 
    $ zip [0..n] (take (fromIntegral (n+1)) fiblist)
  where fiblist = 0:1:zipWith (+) fiblist (tail fiblist)
        n = maximum nums


main :: IO ()        
main = interact (
    concatMap (\res -> show res ++ "\n") 
    . fib 
    . map (\n-> read n :: Integer) 
    . drop 1 
    . lines)