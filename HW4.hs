{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
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

diff :: [Int] -> [Int] -> [Int]
diff = zipWith (-)

splice :: [String] -> [String] -> [String]
splice = (\x y -> (zipWith (++) (zipWith (++) x y) x))

firstStop :: String -> String
firstStop = takeWhile (/= '.')

boundRange :: Integer -> [Integer] -> [Integer]
boundRange x= takeWhile (/= x)

