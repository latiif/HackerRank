balancedParens :: Int -> [String]
balancedParens 0 = [""]
balancedParens n = [ "(" ++ x ++ ")" ++ y
                     | m <- [0..n-1] , x <- balancedParens m , y <- balancedParens (n-1-m) ]