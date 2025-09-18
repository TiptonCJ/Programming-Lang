mapPair :: (a -> b -> c) -> [(a, b)] -> [c]
mapPair f = map (uncurry f)

mapPair' :: (a -> b -> c) -> [(b, a)] -> [c]
mapPair' f = map (uncurry (flip f))

digitsOnly :: [Integer] -> [Integer]
digitsOnly list = filter (>=0)  (filter (<=9) list)

removeXs :: [String] -> [String]
removeXs = filter ((/= "X") . take 1)

sqLens :: [String] -> [Integer]
sqLens = map ((^2) . fromIntegral. length)

bang :: [String] -> [String]
bang = map (++ "!")

diff :: [Integer] -> [Integer] -> [Integer]
diff = zipWith (-)

splice :: [String] -> [String] -> [String]
splice = (\x y -> (zipWith (++) (zipWith (++) x y) x))

firstStop :: String -> String
firstStop = takeWhile (/= '.')

boundRange :: Integer -> [Integer] -> [Integer]
boundRange x= takeWhile (\n -> n >= (-x) && n <= x)

exists :: (a -> Bool) -> [a] -> Bool
exists f [] = False
exists f (x:xs) | f x = True
                | otherwise = exists f xs

exists' :: (a -> Bool) -> [a] -> Bool
exists' f = foldl (\acc x -> if f x then True else acc) False

noDups :: Eq a => [a] -> [a] 
noDups [] = []
noDups (x:xs) | x `elem` xs = x : noDups (filter (/= x) xs)
              | otherwise = x : noDups xs

noDups' :: Eq a => [a] -> [a] 
noDups' = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) []

countOverflow :: Integer -> [String] -> Integer
countOverflow x [] = 0
countOverflow x (y:ys)
  | length y > fromInteger x = 1 + countOverflow x ys
  | otherwise  = countOverflow x ys

countOverflow' :: Integer -> [String] -> Integer
countOverflow' x = foldl (\acc y -> if length y > fromInteger x then acc + 1 else acc) 0

concatList :: [[a]] -> [a]
concatList [] = []
concatList (x:xs) = x ++ concatList xs

concatList' :: [[a]] -> [a]
concatList' = foldl (\acc x -> acc ++ x) []

bindList :: (a -> [b]) -> [a] -> [b]
bindList f [] = []
bindList f (x:xs) = f x ++ bindList f xs

bindList' :: (a -> [b]) -> [a] -> [b]
bindList' f = foldl (\acc x -> acc ++ f x) []