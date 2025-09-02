minList :: [Integer] -> Integer
minList [] = 0
minList x = if length x == 1 then head x else minimum x

addAbs :: [Integer] -> Integer
addAbs [] = 0
addAbs (x:xs)
    | x < 0 = addAbs xs + (x * (-1))
    | otherwise = addAbs xs + x

existsOdd :: [Integer] -> Bool
existsOdd [] = False
existsOdd x = any odd x

findOdd :: [Integer] -> Maybe Integer
findOdd [] = Nothing
findOdd (x:xs) 
    | odd x = Just x
    | otherwise = findOdd xs

removeEmpty :: [String] -> [String]
removeEmpty [] = []
removeEmpty (x:xs)
    | null x = removeEmpty xs
    | otherwise = x : removeEmpty xs

subtractEach :: [(Integer, Integer)] -> [Integer]
subtractEach [] = []
subtractEach [(x,y)] = [x - y]
subtractEach ((x,y):xs) = (x-y) : subtractEach xs

makeGreeting :: Maybe String -> String
makeGreeting Nothing = "Hello!"
makeGreeting (Just name) = "Hello, " ++ name ++ "!"

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x:xs) = x : catMaybes xs

classify :: [Either a b] -> ([a], [b])
classify [] = ([],[])
classify (Left x:xs) = let (ls, rs) = classify xs in (x:ls, rs)
classify (Right x:xs) = let (ls, rs) = classify xs in (ls, x:rs)


