-- https://www.codewars.com/kata/are-they-the-same/train/haskell

import Data.List

comp :: [Integer] -> [Integer] -> Bool
comp as bs = sort (map (^2) as) == sort bs
