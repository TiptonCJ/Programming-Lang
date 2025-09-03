import System.Win32 (xBUTTON1)
f :: Double -> Double
f x = 2*x + 1

g :: Double -> Double -> Double
g x y = x**y + sqrt y * sin (pi * x)

h1 :: Double -> Double
h1 x = if
        x < 0
     then
        x**2
    else
        2 * x + 1

h2 :: Double -> Double -> Double
h2 x y
  | x**2 < 10 = 0
  | (x**2 >= 10) && (x + y <100) = y**2
  | otherwise = 2**x - y**2

sumUp :: Integer -> Integer
sumUp x = sum [0..x]

sumUp' :: Integer -> Integer
sumUp' x = if
            x <= 0
        then
            0
        else
            x + sumUp' (x-1)

sumSquares :: Integer -> Integer
sumSquares x = sum [x^2 | x <- [1..x]]

sumSquares' ::  Integer -> Integer
sumSquares' x = if
                    x <= 0
                then
                    0
                else
                    x^2 + sumSquares' (x-1)

sumOddSquares :: Integer -> Integer
sumOddSquares x = sum [x^2 | x <- [1..x], odd x]

sumOddSquares' ::  Integer -> Integer
sumOddSquares' x
  | x <= 0 = 0
  | even x = sumOddSquares' (x-1)
  | otherwise = x^2 + sumOddSquares' (x-1)

sumOddSquaresRange :: Integer -> Integer -> Integer
sumOddSquaresRange x y = sum[x^2 | x <-[x..y], odd x]

sumOddSquaresRange' :: Integer -> Integer -> Integer
sumOddSquaresRange' x y
    | y <= 0 = 0
    | y < x = 0
    | even y = sumOddSquaresRange' x (y-1)
    | otherwise = y^2 + sumOddSquaresRange' x (y-1)

collatz :: Integer -> Integer
collatz x
    | x == 0 || x == 1 = 1
    | even x = collatz(div x 2)
    | odd x = collatz(3 * x + 1)

myNum :: Integer
myNum = 100
myList :: [Integer]
myList = [1..100]
collatzCheck :: [Integer]
collatzCheck = [collatz x | x <- myList]

getSecond :: [String] -> String
getSecond x = x !! 1

makePalindrome :: String -> String
makePalindrome x = x ++ reverse x

skip3 :: String -> String
skip3 = drop 3

find7 :: [Integer] -> Bool
find7 x = 7 `elem` x