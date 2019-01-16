import Data.List



anagrams :: String -> [String] -> [String]
anagrams w ws = [ anagram | anagram <- ws , isAnAnagram w anagram ]


isAnAnagram :: String -> String -> Bool
isAnAnagram w1 w2 = (length w1) == (length w2) && (sort $ nub w1) == (sort $ nub w2)