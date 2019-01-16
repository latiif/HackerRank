countSmileys :: [String] -> Int
countSmileys smiles =length $ filter (==True) $ map isSmiley smiles

eyes = ":;"
nose = "-~ "
mouth = ")D"

isSmiley :: String -> Bool
isSmiley smile = smile `elem` map (filter (/=' ')) [e:n:[m] | e<- eyes, n<- nose, m<-mouth]