import Data.List


getUnique :: [Int] -> Int
getUnique nums = (concat $filter (\n -> (length n) ==1 ) $ group $ sort nums) !! 0

