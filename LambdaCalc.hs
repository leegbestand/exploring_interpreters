module LambdaCalc where

import Data.List

type V = String

data Expr = Var V | Abstraction V Expr | Application Expr Expr deriving (Eq)
instance Show Expr where
    show (Var v) = v
    show (Abstraction v e) = "\\" ++ v ++ " -> " ++ show e
    show (Application e1 e2) = "(" ++ show e1 ++ ")(" ++ show e2 ++ ")"


free :: Expr -> [V]
free (Var v) = [v]
free (Abstraction v e) = free e \\ [v]
free (Application e1 e2) = free e1 ++ free e2

bound :: Expr -> [V]
bound (Var v) = []
bound (Abstraction v e) = v : bound e
bound (Application e1 e2) = bound e1 ++ bound e2


-- See 2.1 definition: https://plato.stanford.edu/entries/lambda-calculus/#Red
-- Substitute -> With -> In
substitution :: V -> Expr -> Expr -> Expr
substitution x m (Var y) = if x == y then m else Var y
substitution x m (Application a b) = Application (substitution x m a) (substitution x m b)
substitution x m (Abstraction y a) = if x == y then Abstraction y a else Abstraction y (substitution x m a)

availableVar :: [V] -> V -> V
availableVar free v = if v' `elem` free then availableVar free v' else v'
    where v' = v ++ "'"

-- derivative of the alpha to take into account extra free variables,
--  is a helper for beta reduction to ensure no-capture proviso(see: https://plato.stanford.edu/entries/lambda-calculus/#Red)
-- using alpha reduction.
alpha' :: [V] -> V -> Expr -> Expr
alpha' _ _ (Var v) = Var v
alpha' exprFree v (Application e1 e2) = Application (alpha' exprFree v e1) (alpha' exprFree v e2)
alpha' exprFree tochange (Abstraction x m) = 
    if x == tochange 
    then Abstraction newvar (substitution x (Var newvar) (alpha' exprFree tochange m))
    else Abstraction x (alpha' exprFree tochange m)
    where allfree = exprFree ++ free m
          newvar = availableVar allfree x

alpha :: V -> Expr -> Expr
alpha = alpha' []

beta' :: [V] -> [V] -> Expr -> Expr
beta' _ [] e = e
beta' freelist (x:xs) e = beta' freelist xs (alpha' freelist x e)

beta :: Expr -> Expr
beta (Application (Abstraction x e1) e2) = substitution x e2 (beta' f (b \\ (b \\ f)) e1)
    where f = free e2
          b = bound e1
beta (Application e1 e2) = Application (beta e1) e2
beta e = e

eval :: Expr -> IO()
eval e = do 
    print e
    if e' == e 
    then return ()
    else eval e'
    where e' = beta e

freeBound :: Expr
freeBound = Application (Abstraction "x" (Abstraction "y" (Var "x"))) (Var "y")

s :: Expr
s = Abstraction "x" (Abstraction "y" (Abstraction "z" (Application (Application (Var "x") (Var "z")) (Application (Var "y") (Var "z")))))

k :: Expr
k = Abstraction "x" (Abstraction "y" (Var "x"))

i :: Expr
i = Abstraction "x" (Var "x")

b :: Expr
b = Abstraction "x" (Abstraction "y" (Abstraction "z" (Application (Var "x") (Application (Var "y") (Var "z")))))

y :: Expr
y = Abstraction "f" (Application (Abstraction "x" (Application (Var "f") (Application (Var "x") (Var "x")))) (Abstraction "x" (Application (Var "f") (Application (Var "x") (Var "x")))))

smallOmega :: Expr
smallOmega = Abstraction "x" (Application (Var "x") (Var "x"))

-- Eval equals itself.
idOfId :: Expr
idOfId = Application i i

-- Eval is infinite.
yId :: Expr
yId = Application y i
