{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use map" #-}
import Control.Concurrent.STM (check)
import System.Win32 (COORD(xPos))
checkbound :: [String] -> Bool
checkbound = foldr (\ x -> (&&) (length x <= 20)) True

fold :: (a -> b -> b) -> b -> [a] -> b
fold rf bc [] = bc
fold rf bc (x:xs) = rf x (fold rf bc xs)

findPalindrome, findFromFold :: [String] -> Maybe String
findPalindrome [] = Nothing
findPalindrome (x:xs) = if x == reverse x then Just x else findPalindrome xs

findFromFold = foldr (\x y -> if x == reverse x then Just x else y) Nothing

sumLengthsAndLongest, sameFromFold :: [String] -> (Int, String)
sumLengthsAndLongest [] = (0, "")
sumLengthsAndLongest (x:xs) = let reccall = sumLengthsAndLongest xs
                                in if length x > length (snd reccall)
                                    then (length x + fst reccall, x)
                                    else (length x + fst reccall, snd reccall)

sameFromFold = foldr (\x (r1, r2) -> if length x > length r2 then (length x + r1, x)
                                                                else (length x + r1, r2))
                (0, "")

subset :: (Eq a) => [a] -> [a] -> Bool
subset [] ys = True
subset (x:xs) ys = elem x ys && subset xs ys

subsetFromFold :: (Eq a) => [a] -> [a] -> Bool
subsetFromFold list1 list2 = foldr (\x y -> elem x list2 && y) True list1

mapFromFold :: (a -> b) -> [a] -> [b]
-- mapFromFold f [] = []
-- mapFromFold f (x:xs) = f x : map f xs

--mapFromFold f = foldr (\x y -> f x : y) []
mapFromFold f = foldr ((:) . f) []