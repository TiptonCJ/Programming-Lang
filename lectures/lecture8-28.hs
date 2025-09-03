allStringsOdd :: [String] -> Bool
allStringsOdd []     = True
allStringsOdd (x:xs) = allStringsOdd xs && length x `mod` 2 == 1

existsOddString :: [String] -> Bool
existsOddString [] = False
existsOddString (x:xs) = existsOddString xs || odd (length x)

concatStrings :: [String] -> String
concatStrings [] = ""
concatStrings (x:xs) = x ++ concatStrings xs

addList :: [Integer] -> Integer
addList [] = 0
addList (x:xs) = x + addList xs

mulList :: [Integer] -> Integer
mulList [] = 1
mulList (x:xs) = x * mulList xs

checkMultiple :: Integer -> [Integer] -> Bool
checkMultiple n [] = False
checkMultiple n (x:xs) = checkMultiple n xs || x `mod` n == 0

findMultiple :: Integer -> [Integer] -> Integer
findMultiple n [] = -1
findMultiple n (x:xs) = if x `mod` n == 0 then x else findMultiple n xs

findMultipleMaybe :: Integer -> [Integer] -> Maybe Integer
findMultipleMaybe n [] = Nothing
findMultipleMaybe n (x:xs) = if x `mod` n == 0 then Just x else findMultipleMaybe n xs

addComponents :: [(Integer, Integer)] -> [Integer]
addComponents [] = []
addComponents [(x,y)] = [x + y]
addComponents ((y,z):xs) = (y+z) : addComponents xs