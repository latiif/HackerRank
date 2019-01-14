digpow :: Integer -> Integer -> Integer
digpow n p = if isDivisionInteger lhs n then k else -1
   where
     lhs = raiseToPower (decompose n) p
     k = lhs `div` n


-- decomposes a number           
decompose ::    Integer    -- Number of multiple digits t.ex 98
            ->  [Integer]  -- its digits t.ex [9,8]
decompose = reverse . decompose'

decompose' :: Integer -> [Integer]
decompose' 0 = []
decompose' x = (x `mod` 10):decompose' (x `div` 10)



raiseToPower :: [Integer] -- digits of a number
             -> Integer   -- p
             -> Integer   -- res           
raiseToPower [] _ = 0
raiseToPower (d:ds) p = (d^p) + raiseToPower ds (p+1)            



isDivisionInteger :: Integer -> Integer -> Bool
isDivisionInteger a b = isInt p1
    where
         p1 = fromIntegral a / fromIntegral b
         isInt x = x == fromInteger (round x)