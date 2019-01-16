sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = filter (\x -> x == getSumComponents x) [a..b]

getSumComponents :: Int  -> Int
getSumComponents n = sum $ zipWith (\x p -> x^p) components [1..]
    where 
    getComponents' ::  Int -> [Int]
    getComponents' 0  = []
    getComponents' x  = (x `mod` 10):getComponents' (x `div` 10)
    components = reverse $ getComponents' n
