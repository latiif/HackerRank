
import Data.List

ops:: [(Int,Int,Int)]
ops = [(2,3,603),(1,1,286),(4,4,882)]


operate :: [(Int,Int,Int)] -> [(Int,Int)]
operate [] = []
operate ((a,b,k):res) = (a,k):(b+1,-k):operate res


sortLedger :: [(Int,Int)] -> [Int]
sortLedger ledger =  map (\category -> sum (map snd category)) grouped 
    where sorted = sortOn (\(idx,val) -> idx) ledger
          grouped = groupBy (\(idx1,val1) (idx2,val2) -> idx1==idx2) sorted

findBiggestPartialSum :: [Int] -> Int
findBiggestPartialSum ledger = maximum (scanl (+) 0 ledger)


readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

createOps :: [String] -> [(Int,Int,Int)]
createOps strs = map (\line -> let abk = words line in (read(abk!!0)::Int,read(abk!!1)::Int,read(abk!!2)::Int)) strs    

main :: IO ()
main = do
    nmTemp <- getLine
    let nm = words nmTemp

    let n = read (nm !! 0) :: Int

    let m = read (nm !! 1) :: Int
    queriesTemp <- readMultipleLinesAsStringArray m
    putStrLn (show $ findBiggestPartialSum $ sortLedger $ operate  (createOps queriesTemp))
