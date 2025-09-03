--Bubblesort

bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x:y:zs)
    | x > y = y : bubble (x:zs)
    |otherwise = x : bubble(y:zs)

bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort xs = until sorted bubble xs
    where sorted ys = bubble ys == ys


--subString
isSubstring :: String -> String -> Bool
isSubstring [] ys = True
isSubstring xs [] = False
isSubstring xs (y:ys) 
    | isPrefix xs (y:ys) = True
    |otherwise = isSubstring xs ys


isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix xs ys = xs == take (length xs) ys

--genPrefix
genPrefix :: String -> [String]
genPrefix "" = []
genPrefix x = reverse (map reverse (genTails (reverse x)))


genTails :: String -> [String]
genTails "" = []
genTails (x:xs) =  (x:xs) : genTails xs

--genSubstrings
genSubstrings :: String -> [String]
genSubstrings "" = []
genSubstrings xs = "" : concatMap genTails (genPrefix xs)

--replacePrefix
replacePrefix :: (String,String) -> String -> String
replacePrefix (x,[]) zs = zs
replacePrefix (x,y) [] = []
replacePrefix (x,y) zs = y ++ drop (length x) zs

replaceString :: (String,String) -> String -> String



--tester below

