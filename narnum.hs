narcissistic :: Integral n => n -> Bool
narcissistic x = x' ==  (getComponents x')
    where x' = fromIntegral x

getComponents :: Int  -> Int
getComponents n = sum $ map (^len) components
    where 
    getComponents' :: Int -> [Int]
    getComponents' 0 = []
    getComponents' x = (x `mod` 10):getComponents' (x `div` 10)
    components = getComponents' n
    len = length components