fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

main :: IO ()
main = do
    line <- getLine
    let n = (read line) :: Integer
    putStrLn $ show $ fact n