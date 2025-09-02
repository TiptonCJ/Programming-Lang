sumCubes :: Integer -> Integer
sumCubes x = sum [x^3 | x <- [1..x]]

equalEnds :: String -> Bool
equalEnds x = head x == last x

equalLength :: String -> Int -> Bool
equalLength x y = length x <= y

filterDigits :: [Integer] -> [Integer]
filterDigits xs = [x | x <- xs, 0 <= x && x <= 9]