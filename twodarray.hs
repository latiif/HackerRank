-- 2D Array DS


-- Retrieves an element from a 2d array
getEl :: (Int,Int) -> [[Int]] -> Int
getEl (i,j) m = (m !! i) !! j


-- Mask used for hourglass coordinate
mask:: [(Int, Int)]
mask = [(0,0),(0,1),(0,2),(1,1),(2,0),(2,1),(2,2)]

-- Get an hourglass given its start indices
getHg :: Int -> Int -> [[Int]] -> [Int]
getHg i j m = map (\index -> getEl index m) indices
    where indices = map (\(x,y)->(x+i,y+j)) mask

-- Get all possible hourglasses    
getHgs :: [[Int]] -> [[Int]]
getHgs m = [getHg i j m | j<- [0..3], i<-[0..3]] 

-- Returns the maximum sum of hourglass components
solve :: [[Int]] -> Int
solve m = maximum $ map sum (getHgs m)

-- Reads the 2d Integer array from String
getMatrix :: String -> [[Int]]
getMatrix str = map (map (\lst -> read lst :: Int)) $ map words $ lines str

-- Wrapper for IO 
doSolve :: String -> String
doSolve str = show $ solve m
    where m = getMatrix str

-- Main program
main :: IO ()
main = interact doSolve


