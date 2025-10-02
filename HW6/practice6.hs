mapAppend :: (a -> [b]) -> [a] -> [b]
mapAppend f [] = []
mapAppend f (x:xs) = f x ++ mapAppend f xs

mapAppend' :: (a -> [b]) -> [a] -> [b]
mapAppend' f = foldl (\acc x -> acc ++ f x) []

mapAppend'' :: (a -> [b]) -> [a] -> [b]
mapAppend'' f = foldr ((++) . f) []

addLetter :: Char -> [String] -> [String]
addLetter _ [] = []
addLetter x (y:ys) = (x : y) : addLetter x ys

addLetters :: [Char] -> [String] -> [String]
addLetters [] [] = []
addLetters [] y = []
addLetters xs [] = []
addLetters (x:xs) y = addLetter x y ++ addLetters xs y

makeWords :: [Char] -> Integer -> [String]
makeWords _ 0 = [""]
makeWords xs y = addLetters xs (makeWords xs (y-1))

update :: Eq a => (a,b) -> [(a,b)] -> [(a,b)]
update (key,val) [] = [(key,val)]
update (key,val) ((x,y):xs)
  | key == x  = (key,val) : xs
  | otherwise = (x,y) : update (key,val) xs

  

