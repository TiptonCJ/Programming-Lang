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

  






type Vars = String
type Env = [(Vars,Bool)]
data Prop = Var Vars | Const Bool | And Prop Prop | Or Prop Prop | Not Prop
    deriving (Eq, Show)

prop1 = Var "X" `And` Var "Y" -- X /\ Y
prop2 = Var "X" `Or` Var "Y" -- X \/ Y
prop3 = Not (Var "X") `Or` (Var "Y") -- !X \/ Y
prop4 = Not (Var "X") `Or` Not (Var "Y") -- !X \/!Y

noDups :: (Eq a) => [a] -> [a]
noDups [] = []
noDups (x:xs) = x : noDups (filter (/= x) xs)

fv :: Prop -> [Vars]
fv (Var v) = [v]
fv (Const _) = []
fv (And x y) = noDups (fv x ++ fv y)
fv (Or x y) = noDups (fv x ++ fv y)
fv (Not x) = noDups (fv x)

countOccurs :: Vars -> Prop -> Integer
countOccurs x (Var v) = if x == v then 1 else 0
countOccurs x (Const _) = 0
countOccurs x (And y z) = countOccurs x y + countOccurs x z
countOccurs x (Or y z) = countOccurs x y + countOccurs x z
countOccurs x (Not y) = countOccurs x y

setTrue :: Vars -> Prop -> Prop
setTrue v (Var x) = if x == v then Const True else Var x
setTrue v (Const x) = Const False
setTrue v (And x y) = And (setTrue v x) (setTrue v  y)
setTrue v (Or x y) = Or (setTrue v x) (setTrue v  y)
setTrue v (Not x) = Not (setTrue v x)

lookUp :: Vars -> Env -> Bool
lookUp v [] = error "The variable is not present in the Env."
lookUp v ((x,y):list) = if v == x then y else lookUp v list

eval :: [(Vars,Bool)] -> Prop -> Bool
eval env (Var v) = lookUp v env
eval env (Const x) = x
eval env (And x y) = eval env x && eval env y
eval env (Or x y) = eval env x || eval env y
eval env (Not x) = not (eval env x)

evalList :: Prop -> [Env] -> Bool
evalList _ [] = False
evalList f (env:list) = eval env f || evalList f list

extendEnv :: [Env] -> Vars -> [Env]
extendEnv [] v = []
extendEnv (env:list) v = ([(v,False) : env] ++ [(v,True) : env]) ++ extendEnv list v

genEnvs :: [Vars] -> [Env]
genEnvs list = foldl extendEnv [[]] (reverse list)

sat :: Prop -> Bool
sat f = evalList f (genEnvs (fv f))

checkEq :: Prop -> Prop -> Bool
checkEq f1 f2 = 
    let vars = noDups (fv f1 ++ fv f2)
        envs = genEnvs vars 
    in all (\env -> eval env f1 == eval env f2) envs

-- testing below

