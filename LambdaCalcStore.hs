module LambdaCalcStore where

import qualified Data.Map as Map
import Control.Monad.Trans.State.Lazy
import Data.List

type V = String
type Store = Map.Map V Expr
type StoreM = State Store
data Config = Config { cfgStore :: Store } deriving (Show, Eq)


data Expr = Var V | Abstraction V Expr | Closure Store Expr | Application Expr Expr deriving (Eq)
instance Show Expr where
    show (Var v) = v
    show (Abstraction v e) = "\\" ++ v ++ " -> " ++ show e
    show (Application e1 e2) = "(" ++ show e1 ++ ")(" ++ show e2 ++ ")"
    show (Closure s e) = show e ++ " in " ++ show s




eval :: Expr -> StoreM Expr
eval (Application (Abstraction x e1) e2) = do
    e' <- eval (Abstraction x e1)
    eval (Application e' e2)
eval (Application (Closure env (Abstraction x e1)) e2) =
    return $ evalState (eval e1) (Map.insert x e2 env)
eval (Application e1 e2) = do
    e1' <- eval e1
    e2' <- eval e2
    eval (Application e1' e2')
eval (Abstraction arg e) = do
    m <- get
    return (Closure m (Abstraction arg e))
eval (Var x) = do
    m <- get
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