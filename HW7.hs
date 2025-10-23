import Data.Char 


type Vars = String
type Value = Bool
type Env = [(Vars,Bool)]
data Prop = Var Vars | Const Bool | And Prop Prop | Or Prop Prop | Not Prop
        | Imp Prop Prop | Iff Prop Prop | Xor Prop Prop
        deriving (Show,Eq)


-- prop1 = Var "X" `And` Var "Y" -- X /\ Y
-- prop2 = Var "X" `Imp` Var "Y" -- X -> Y
-- prop3 = Not (Var "X") `Or` (Var "Y") -- !X \/ Y
-- prop4 = Not (Var "X") `Iff` Not (Var "Y") -- !X <-> !Y

noDups :: (Eq a) => [a] -> [a]
noDups [] = []
noDups (x:xs) = x : noDups (filter (/= x) xs)

fv :: Prop -> [Vars]
fv (Var v) = [v]
fv (Const _) = []
fv (And x y) = noDups (fv x ++ fv y)
fv (Or x y) = noDups (fv x ++ fv y)
fv (Not x) = noDups (fv x)
fv (Imp x y) = noDups (fv x ++ fv y)
fv (Iff x y) = noDups (fv x ++ fv y)
fv (Xor x y) = noDups (fv x ++ fv y)

lookUp :: Vars -> Env -> Bool
lookUp v [] = error "The variable is not present in the Env."
lookUp v ((x,y):list) = if v == x then y else lookUp v list

eval :: Env -> Prop -> Bool
eval env (Var v) = lookUp v env
eval env (Const x) = x
eval env (And x y) = eval env x && eval env y
eval env (Or x y) = eval env x || eval env y
eval env (Not x) = not (eval env x)
eval env (Imp x y) = not (eval env x) || eval env y
eval env (Iff x y) = eval env x == eval env y
eval env (Xor x y) = eval env x /= eval env y

evalList :: Prop -> [Env] -> Bool
evalList _ [] = False
evalList f (env:list) = eval env f || evalList f list

evalListAll :: Prop -> [Env] -> Bool
evalListAll _ [] = True
evalListAll f (env:list) = eval env f && evalListAll f list

extendEnv :: [Env] -> Vars -> [Env]
-- extendEnv [] v = []
extendEnv envs v = [(v, True):env | env <- envs] ++ [(v, False):env | env <- envs]


genEnvs :: [Vars] -> [Env]
genEnvs list = foldl extendEnv [[]] (reverse list)

-- A formula is satisfiable if there exists a variable environment making the formula true. --
sat :: Prop -> Bool
sat f = evalList f (genEnvs (fv f))

-- A formula is a contradiction if no variable environment makes the formula true. --
contra :: Prop -> Bool
contra f = not (sat f)

-- A formula is a tautology if every variable environment makes the formula true. --
tauto :: Prop -> Bool
tauto f = evalListAll f (genEnvs (fv f))

findSat :: Prop -> Maybe Env
findSat f = findEnv (genEnvs (fv f))
        where
        findEnv :: [Env] -> Maybe Env
        findEnv [] = Nothing
        findEnv (env:list)
                | eval env f = Just env
                | otherwise = findEnv list

findRefute :: Prop -> Maybe Env
findRefute f = findEnv (genEnvs (fv f))
  where
    findEnv :: [Env] -> Maybe Env
    findEnv [] = Nothing
    findEnv (env:list)
      | not (eval env f) = Just env
      | otherwise = findEnv list

classify :: Prop -> String
classify f
        | tauto f = "tautology"
        | contra f = "contradiction"
        | otherwise = "contingency"

checkEq :: Prop -> Prop -> Bool
checkEq x y = classify (Iff x y) == "tautology"

refuteEq :: Prop -> Prop -> Maybe Env
refuteEq x y = findEnv (genEnvs (noDups (fv x ++ fv y)))
        where 
                findEnv [] = Nothing
                findEnv (env:envs)
                        | not (eval env (Iff x y)) = Just env
                        | otherwise = findEnv envs

-- doesn't work on the first test provided by the homework --


-- binary operators
data BOps = AndOp | OrOp | ImpOp | IffOp | XorOp
        deriving (Show,Eq)

-- the type of tokens
data Token = VSym Vars | CSym Bool | BOp BOps | NotOp | LPar | RPar
        | Err String -- auxiliary token to store unrecognized symbols
        | PB Prop -- auxiliary token to store parsed boolean expressions
                deriving (Show,Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isUpper c = let (restChars, rest) = span isAlphaNum cs
                    name = c:restChars
                in case name of
                     "tt" -> CSym True  : lexer rest
                     "ff" -> CSym False : lexer rest
                     _ -> VSym name  : lexer rest
lexer ('/':'\\':rest) = BOp AndOp : lexer rest
lexer ('\\':'/':rest) = BOp OrOp : lexer rest
lexer ('-':'>':rest) = BOp ImpOp : lexer rest
lexer ('<':'-':'>':rest) = BOp IffOp : lexer rest
lexer ('<':'+' :'>':rest) = BOp XorOp : lexer rest 
lexer (c:cs)
  | isSpace c = lexer cs
  | c == '!' = NotOp : lexer cs
  | c == '(' = LPar : lexer cs
  | c == ')' = RPar : lexer cs
lexer (c:cs) = Err [c] : lexer cs

