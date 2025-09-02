-- fact: takes an Integer and returns the factorial as an Integer
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

-- twice: takes a function and an Intger and runs the function on the Integer twice
twice :: (Integer -> Integer) -> (Integer -> Integer)
twice f x = f (f x)
