multiplesOfSeventeen:: [Integer]
multiplesOfSeventeen = [x | x <- [1..200], mod x 17 == 0]

radius:: Double -> Double -> Double
radius x y = sqrt (x**2 + y**2)

sumEvens:: Integer -> Integer
sumEvens x = sum [x | x <- [1..x], even x]

multiplyEnds :: [Integer] -> Integer
multiplyEnds x 
    | null x = 1
    | otherwise = head x * last x

getLengths:: [String] -> [Int]
getLengths st = [length x | x <- st]

dropLastTwo:: [Integer] -> [Integer]
dropLastTwo x = init (init x)

findEmpty:: [String] -> Bool
findEmpty = any null

checkPalindrome:: String -> Bool
checkPalindrome x = x == reverse x

checkSize:: [Integer] -> Bool
checkSize x = (length x >= 3) && (head x >= 10)

checkAnySize:: Integer -> [Integer] -> Bool
checkAnySize x y = (length y >= fromInteger x) && (head y >= x)

