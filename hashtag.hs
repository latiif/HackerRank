import Data.Char


generateHashtag :: String -> Maybe String
generateHashtag msg | length hashtag > 140 = Nothing
                    | [] == msgWords       = Nothing
                    | otherwise            = Just hashtag
    where
         msgWords = words msg
         hashtag = '#':(concatMap (\w-> toUpper (head w):tail w ) $ msgWords)