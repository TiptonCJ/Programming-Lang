addList :: [Integer] -> Integer
addList [] = 0
addList (x:xs) = x + addList xs


addMaybes :: [Maybe Integer] -> Integer
addMaybes [] = 0
addMaybes (Nothing:xs) = addMaybes xs
addMaybes (Just x:xs) = x + addMaybes xs

addNumbers :: [Either Integer Double] -> Double
addNumbers [] = 0
addNumbers (Left n :xs) = fromInteger n + addNumbers xs
addNumbers (Right d :xs) = d + addNumbers xs

--Insertion sort
insertionSort :: [Integer] -> [Integer]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

insert :: Integer -> [Integer] -> [Integer]
insert n [] = [n]
insert n (x:xs)
    | n <= x = n : x : xs
    | otherwise = x : insert n xs

--merge sort
mergeSort :: (Ord a) => [a] -> [a] -> [a]
mergeSort [] ys = ys
mergeSort xs [] = xs
mergeSort (x:xs) (y:ys)
    | x <= y = x : mergeSort xs (y:ys)
    | y <= x = y : mergeSort (x:xs) ys


split :: [a] -> ([a], [a])
split [] = ([],[])
split [x] = ([x],[])
split(x:y:zs) =
    let rest = (split zs)
    in (x : fst rest, y : snd rest)

merge :: (Ord a) => [a] -> [a]
merge [] = []
merge [x] = [x]
merge xs = mergeSort (merge x1) (merge x2) where (x1,x2) = split xs