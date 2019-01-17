humanReadable :: Int -> String
humanReadable x = padWithZeros hh ++ ":" ++ padWithZeros mm ++ ":" ++ padWithZeros ss
    where
        hh = x `div` 3600
        mm = (x `mod` 3600) `div` 60
        ss = (x `mod` 3600) `mod` 60 
        padWithZeros :: Int -> String
        padWithZeros n = replicate (2-len) '0' ++ nStr
            where 
                nStr = show n
                len = length nStr
