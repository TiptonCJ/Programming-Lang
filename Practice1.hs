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