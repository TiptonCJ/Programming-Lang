type Vars = String
type Env = [(Vars,Bool)]
data Prop = Var Vars | Const Bool | And Prop Prop | Or Prop Prop | Not Prop
    deriving Show

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

