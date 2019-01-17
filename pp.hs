import Test.QuickCheck
 
isPP :: Integer -> Maybe (Integer, Integer)
isPP n' = case [(round m, round k) | m <-[2..n],  let k = logBase m n :: Float, k > 1 ,isInt k  , m**k==n] of
        [] -> Nothing
        (x:_) -> Just x
    where
        n = fromInteger n'
        isInt x = x == fromInteger (round x)


prop_pp :: Integer -> Bool
prop_pp n' = case isPP n of
    Just (m,k) -> (m^k) == n
    _          -> not (n `elem` pps)
    where n = abs n'


pps = [4, 8, 9, 16, 25, 27, 32, 36, 49, 64, 81, 100, 121, 125, 128, 144, 169, 196, 216, 225, 243, 256, 289, 324, 343, 361, 400, 441, 484]




test :: [Integer] -> IO ()
test [] = putStrLn "Done"
test (x:xs) = do
    putStrLn ("Testing for \t"++ (show x))
    case isPP x of
        Nothing -> putStrLn "It's not a perfect power"
        Just (m,k)   -> putStrLn $ show (m,k) ++ "\t\t" ++ show (m^k)
    test xs
    

