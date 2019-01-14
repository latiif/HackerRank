-- sum by factors

import Data.List

sumOfDivided :: [Integer] -> [(Integer, Integer)]
sumOfDivided xs = getFactors
    where
        getFactors = concatMap primeFactors xs



primeFactors :: Integer -> [(Integer,Integer)]
primeFactors n = map (\primefactor -> (primefactor,n)) $ nub $ primeFactors' n


primeFactors':: Integer -> [Integer]
primeFactors' n =
    case factors of
      [] -> [n]
      _  -> factors ++ primeFactors' (n `div` (head factors))
    where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]