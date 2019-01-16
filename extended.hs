import Data.List

expandedForm :: Int -> String
expandedForm  = disp . getComponents


disp :: [Int] -> String
disp []     = ""
disp [x]    = show x
disp (x:xs) = (show x)++" + "++disp(xs)

getComponents :: Int  -> [Int]
getComponents n = filter (/=0) $ reverse $ getComponents' n 0
    where 
    getComponents' :: Int -> Int -> [Int]
    getComponents' 0 _ = []
    getComponents' x p = ((x `mod` 10) * 10^p):getComponents' (x `div` 10) (p+1)

 