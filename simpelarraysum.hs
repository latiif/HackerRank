-- Simple Array Sum


readIntArray :: String -> [Int]
readIntArray line = map (\n -> read n::Int) $ words line

main :: IO ()
main = do
    n <- getLine
    nums <- getLine
    print (sum $ readIntArray nums)