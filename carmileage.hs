import Data.Char
import Data.List

data Answer = No | Almost | Yes deriving (Show, Read, Eq, Ord)




isInteresting :: Integer -> [Integer] -> Answer
isInteresting x xs | performChecks x xs                                = Yes
                   | performChecks (x+2) xs || performChecks (x+1) xs  = Almost
                   | otherwise                                         = No


performChecks :: Integer -> [Integer] -> Bool
performChecks x awesome = isAwesome x awesome || any (\check -> check x) checks
    where
    checks = [followedByZeros,sameDigits,increasingSequential,decreasingSequential,isPalindrome]

followedByZeros :: Integer -> Bool
followedByZeros num = (3<= length numstr) &&  all (=='0') (tail numstr)
    where 
        numstr = show num 

sameDigits :: Integer -> Bool
sameDigits num = (3 <= length numstr) && all (==fstDigit) (tail numstr)
    where
        numstr = show num
        fstDigit = head numstr


increasingSequential :: Integer -> Bool
increasingSequential num = (3 <= len) && numstr == take len ([fstDigit..'9'] ++ ['0'])
        where 
            numstr = show num
            len = length numstr
            fstDigit = head numstr

decreasingSequential :: Integer -> Bool
decreasingSequential num = (3 <= len) && numstr == take len ([fstDigit,chr (ord fstDigit - 1)..'1'] ++ ['0'])
    where 
        numstr = show num
        len = length numstr
        fstDigit = head numstr

isPalindrome :: Integer -> Bool
isPalindrome num = (3 <= len) && numstr == numstr'
        where
            numstr = show num
            len  = length numstr
            numstr' = reverse numstr


isAwesome :: Integer -> [Integer] -> Bool
isAwesome num  awesome = (length $ show num) >=3  && num `elem` awesome

