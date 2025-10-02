data AExpr = Var String | Num Integer | Add AExpr AExpr
    | Sub AExpr AExpr | Mul AExpr AExpr | Div AExpr AExpr
    deriving Show

expr1, expr2 :: AExpr
expr1 = Mul (Add (Num 1) (Num 2)) (Sub (Num 5) (Num 3))
expr2 = Mul (Add (Num 1) (Var "x")) (Sub (Num 5) (Var "y"))

getVars :: AExpr -> [String]
getVars (Var v) = [v]
getVars (Num n) = []
getVars (Add e1 e2) = getVars e1 ++ getVars e2
getVars (Sub e1 e2) = getVars e1 ++ getVars e2
getVars (Mul e1 e2) = getVars e1 ++ getVars e2
getVars (Div e1 e2) = getVars e1 ++ getVars e2

countNums :: AExpr -> Integer
countNums (Var x) = 1
countNums (Num x) = 1
countNums (Add e1 e2) = countNums e1 + countNums e2
countNums (Sub e1 e2) = countNums e1 + countNums e2
countNums (Mul e1 e2) = countNums e1 + countNums e2
countNums (Div e1 e2) = countNums e1 + countNums e2

substNum :: (String, Integer) -> AExpr -> AExpr
substNum (x,n) (Var y) = if x==y then Num n else Var y
substNum (x,n) (Num m) = Num m
substNum (x,n) (Add e1 e2) = Add (substNum (x,n) e1) (substNum (x,n) e2)
substNum (x,n) (Sub e1 e2) = Sub (substNum (x,n) e1) (substNum (x,n) e2)
substNum (x,n) (Mul e1 e2) = Mul (substNum (x,n) e1) (substNum (x,n) e2)
substNum (x,n) (Div e1 e2) = Div (substNum (x,n) e1) (substNum (x,n) e2)

