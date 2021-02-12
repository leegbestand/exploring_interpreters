module Whilelang where

import qualified Data.Map as Map
import Data.List
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import ExploringInterpreter as E

data Literal = LitBool Bool | LitInt Integer deriving (Eq)
instance Show Literal where
    show (LitBool b) = show b
    show (LitInt i) = show i

data Expr = Leq Expr Expr | Plus Expr Expr | LitExpr Literal | Id String deriving (Eq)
instance Show Expr where
    show (Leq e1 e2) = show e1 ++ "<=" ++ show e2
    show (Plus e1 e2) = show e1 ++ "+" ++ show e2
    show (LitExpr lit) = show lit
    show (Id s) = s

data Command = Seq Command Command | Assign String Expr | Print Expr | While Expr Expr Command | Done deriving (Eq)
instance Show Command where
    show (Print e1) = "print(" ++ show e1 ++ ")"
    show Done = "Done"
    show (Assign s e) = s ++ " = " ++ show e
    show (Seq c1 c2) = show c1 ++ "\n" ++ show c2
    show (While e1 e2 c) = "while(" ++ show e2 ++ ") do\n" ++ show c ++ "\nod"

type Store = Map.Map String Literal
type StoreM = State Store
type Output = [String]
data Config = Config { cfgStore :: Store, cfgOutput :: Output } deriving (Show, Eq)

type WhileExplorer = E.Explorer Command Config


evalPlus :: Expr -> Expr -> StoreM Expr
evalPlus (LitExpr (LitInt l1)) (LitExpr (LitInt l2)) = return $ LitExpr $ LitInt (l1 + l2)
evalPlus (LitExpr l1) l2 = do
    l2' <- evalExpr l2
    return (Plus (LitExpr l1) l2')
evalPlus l1 l2 = do
    l1' <- evalExpr l1
    return (Plus l1' l2)

evalLeq :: Expr -> Expr -> StoreM Expr
evalLeq (LitExpr (LitInt l1)) (LitExpr (LitInt l2)) = return $ LitExpr $ LitBool (l1 <= l2)
evalLeq (LitExpr l1) e2 = do
    e2' <- evalExpr e2
    return (Leq (LitExpr l1) e2')
evalLeq e1 e2 = do
    e1' <- evalExpr e1
    return (Leq e1' e2)

evalExpr :: Expr -> StoreM Expr
evalExpr (LitExpr e) = return $ LitExpr e
evalExpr (Plus e1 e2) = evalPlus e1 e2
evalExpr (Leq e1 e2) = evalLeq e1 e2
evalExpr (Id s) = do
    m <- get
    let l = Map.lookup s m
    case l of
        Just lit -> return $ LitExpr lit
        Nothing -> error $ "Invalid Id: " ++ s

evalExpr' :: Expr -> StoreM Expr
evalExpr' (LitExpr e) = return $ LitExpr e
evalExpr' e = do
    e' <- evalExpr e
    evalExpr' e'

store :: String -> Expr -> StoreM Command
store s (LitExpr l) = do
    lut <- get
    put $ Map.insert s l lut
    return Done

evalCommand :: Command -> WriterT [String] StoreM Command
evalCommand (Print e) = do
    x <- (lift . evalExpr') e
    tell [show x]
    return Done
evalCommand (Assign id e) = do
    lit <- (lift . evalExpr') e
    lift $ store id lit
evalCommand (Seq Done c2) = return c2
evalCommand (Seq c1 c2) = do
    c1' <- evalCommand c1
    return $ Seq c1' c2
evalCommand (While (LitExpr (LitBool False)) e2 c) = return Done
evalCommand (While (LitExpr (LitBool True)) e2 c) = return $ Seq c (While e2 e2 c)
evalCommand (While e1 e2 c) = do
    e1' <- (lift . evalExpr') e1
    return $ While e1' e2 c


evalCommand' :: Command -> WriterT [String] StoreM Command
evalCommand' Done = return Done
evalCommand' c = do
    c' <- evalCommand c
    evalCommand' c'

-- Initial configuration in the while language)
initialConfig :: Config
initialConfig = Config {cfgStore = Map.empty, cfgOutput = []}

-- Definitial interpreter for the while language.
definterp :: Command -> Config -> Config
definterp c cfg = cfg {cfgStore = newstore, cfgOutput = cfgOutput cfg ++ newout}
    where ((_, newout), newstore) = runState (runWriterT (evalCommand' c)) (cfgStore cfg)

definterpM :: Command -> Config -> IO Config
definterpM c cfg = do
    print newout
    return cfg {cfgStore = newstore, cfgOutput = cfgOutput cfg ++ newout}
    where ((_, newout), newstore) = runState (runWriterT (evalCommand' c)) (cfgStore cfg)


-- whileLang = (Command, Config, initialConfig, definterp)
--
do_ :: Command -> WhileExplorer -> IO WhileExplorer
do_ (Seq c1 c2) e = do_ c1 e >>= do_ c2
do_ p e = do
    let e' = execute p e
    putStr $ unlines $ cfgOutput (config e') \\ cfgOutput (config e)
    return e'

start :: IO WhileExplorer
start = return whileGraph

session1 :: IO ()
session1 = start >>=
  do_ (assign "x" (intToExpr 1)) >>= 
  do_ (assign "y" (Id "x")) >>= 
  do_ (Print (Id "y")) >>=
  displayDot 

-- Below are some helpers to create a Command and fully evaluate it.
-- Example:
-- ghci> let x = wprint (intToExpr 10) `wseq` (wprint (intToExpr 100) `wseq` wprint (intToExpr 200))
-- ghci> runCommand' x
-- ["10","100","200"]
-- ghci>
runCommand :: Command -> IO()
runCommand c = do
    let ((_, output), _) = runState (runWriterT (evalCommand' c)) Map.empty
    print output


intToExpr :: Integer -> Expr
intToExpr = LitExpr . LitInt

boolToExpr :: Bool -> Expr
boolToExpr = LitExpr . LitBool

while ::  Expr -> Command -> Command
while e = While e e

leq :: Expr -> Expr -> Expr
leq = Leq

wprint :: Expr -> Command
wprint = Print

plus :: Expr -> Expr -> Expr
plus = Plus

assign :: String -> Expr -> Command
assign = Assign

wseq :: Command -> Command -> Command
wseq = Seq



whileGraph :: WhileExplorer
whileGraph = mkExplorerGraph definterp initialConfig

whileTree :: WhileExplorer
whileTree = mkExplorerTree definterp initialConfig

whileStack :: WhileExplorer 
whileStack = mkExplorerStack definterp initialConfig

whileExample = Seq (Assign "x" (intToExpr 0)) (while (Leq (Id "x") (intToExpr 10)) (Seq (Assign "x" (Plus (Id "x") (intToExpr 1))) (Print (Id "x"))))

zero = intToExpr 0

getRef :: WhileExplorer -> Ref
getRef = currRef

getLast :: WhileExplorer -> Gr () Command
getLast = subExecEnv
