-- sum by factors

import Data.List

sumOfDivided :: [Integer] -> [(Integer, Integer)]
sumOfDivided xs = map (\factor -> (fst $ head factor, sum (map snd factor) )) 
                $ groupBy (\a b -> fst a == fst b) 
                $ sort getFactors

    where
        biggest = maximum (map abs xs)
        primes  = primesUnder biggest
        getFactors = concatMap (primeFactors primes) xs



primeFactors :: [Integer] -> Integer -> [(Integer,Integer)]
primeFactors listOfPrimes n = map (\primefactor -> (primefactor,n)) $ nub $ primeFactors' listOfPrimes n


primeFactors':: [Integer] -> Integer -> [Integer]
primeFactors' listOfPrimes n | n<=0 = primeFactors' listOfPrimes (-n)
primeFactors'    []  _ = []
primeFactors' (p:ps) n  | p > n  = []
                        | n `mod` p == 0 = p:primeFactors' ps (removeFactor n p)
                        | otherwise      = primeFactors' ps n
                        where removeFactor :: Integer -> Integer -> Integer
                              removeFactor x factor | x `mod` factor == 0 = removeFactor (x `div` factor) factor
                                                    | otherwise           = x                       



primesUnder :: Integer -> [Integer]
primesUnder x = 2 : primes'
  where isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
        primes' = 3 : filter (isPrime primes') [5, 7 .. x]