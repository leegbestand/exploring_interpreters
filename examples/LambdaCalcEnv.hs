module LambdaCalcEnv where

import qualified Data.Map as Map
import Control.Monad.Trans.State.Lazy
import Control.Monad.Reader
import Data.List

type V = String
type Env = Map.Map V Expr
type EnvM = Reader Env
data Config = Config { env :: Env } deriving (Show, Eq)


data Expr = Var V | Abstraction V Expr | Closure Env V Expr | Application Expr Expr deriving (Eq)
instance Show Expr where
    show (Var v) = v
    show (Abstraction v e) = "\\" ++ v ++ " -> " ++ show e
    show (Application e1 e2) = "(" ++ show e1 ++ ")(" ++ show e2 ++ ")"
    show (Closure s v e) = "\\" ++ v ++ " -> " ++ show e ++ " in " ++ show s




eval :: Expr -> EnvM Expr
eval (Application (Abstraction x e1) e2) = do
    e' <- eval (Abstraction x e1)
    eval (Application e' e2)
eval (Application (Closure env x e1) e2) =
    return $ runReader (eval e1) (Map.insert x e2 env)
eval (Application e1 e2) = do
    e1' <- eval e1
    e2' <- eval e2
    eval (Application e1' e2')
eval (Abstraction arg e) = do
    m <- ask
    return (Closure m arg e)
eval (Var x) = do
    m <- ask
    case Map.lookup x m of
        Just e -> return e
        Nothing -> error $ "Variable " ++ x ++ " not in scope"
eval e = return e


k :: Expr
k = Abstraction "x" (Abstraction "y" (Var "x"))

i :: Expr
i = Abstraction "x" (Var "x")


closureTest :: Expr
closureTest = Abstraction "x" (Abstraction "y" (Application (Var "x") (Var "y")))

-- Binds x to \x -> x
test' :: IO()
test' = print $ runReader (eval (Application closureTest i)) Map.empty

-- Results in the identity function.
test :: IO()
test = print $ runReader (eval (Application (Application closureTest i) i)) Map.empty
