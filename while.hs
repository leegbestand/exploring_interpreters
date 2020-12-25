module Whilelang where

import qualified Data.Map as Map
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class

data Literal = LitBool Bool | LitInt Integer deriving (Eq)
instance Show Literal where
    show (LitBool b) = show b
    show (LitInt i) = show i

data Expr = Leq Expr Expr | Plus Expr Expr | LitExpr Literal | Id String
instance Show Expr where
    show (Leq e1 e2) = (show e1) ++ "<=" ++ (show e2)
    show (Plus e1 e2) = (show e1) ++ "+" ++ (show e2)
    show (LitExpr lit) = show lit
    show (Id s) = show s

data Command = Seq Command Command | Assign String Expr | Print Expr | While Expr Expr Command | Done
instance Show Command where
    show (Print e1) = "Print(" ++ (show e1) ++ ")"
    show (Done) = "Done"
    show (Assign s e) = (show s) ++ " = " ++ (show e)
    show (Seq c1 c2) = "Seq(" ++ (show c1) ++ ", " ++ (show c2) ++ ")"
    show (While e1 e2 c) = "While(" ++ (show e2) ++ "){ " ++ (show c) ++ " }"

type Store = Map.Map String Literal
type StoreM = State Store
type Output = [String]
data Config = Config { cfg_store :: Store, cfg_output :: Output } deriving (Show, Eq)

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
    tell $ [(show x)]
    return Done
evalCommand (Assign id e) = do
    lit <- (lift . evalExpr') e
    c <- lift $ store id lit
    return c
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
initialConfig = Config {cfg_store = Map.empty, cfg_output = []}

-- Definitial interpreter for the while language.
definterp :: Command -> Config -> Config
definterp c cfg = cfg {cfg_store = newstore, cfg_output = (cfg_output cfg) ++ newout}
    where ((_, newout), newstore) = runState (runWriterT (evalCommand' c)) (cfg_store cfg)

-- whileLang = (Command, Config, initialConfig, definterp)


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
intToExpr = (LitExpr . LitInt)

boolToExpr :: Bool -> Expr
boolToExpr = (LitExpr . LitBool)

while ::  Expr -> Command -> Command
while e c = While e e c

leq :: Expr -> Expr -> Expr
leq e1 e2 = Leq e1 e2

wprint :: Expr -> Command
wprint c = Print c

plus :: Expr -> Expr -> Expr
plus e1 e2 = Plus e1 e2

assign :: String -> Expr -> Command
assign s e = Assign s e

wseq :: Command -> Command -> Command
wseq c1 c2 = Seq c1 c2


-- TODO: move to own module.
-- Currently uses a list(stack) as the execution graph.
data ExploringInt programs configs = ExploringInt { expl_definterp :: programs -> configs -> configs, expl_config :: configs, exec_graph :: [(configs, programs)]}

-- Constructor for a exploring interpreter.
buildExplInt :: (a -> b -> b) -> b -> ExploringInt a b
buildExplInt definterp conf = ExploringInt {expl_definterp = definterp, expl_config = conf , exec_graph = []}

explore :: ExploringInt p c -> p -> ExploringInt p c
explore int prog = int { expl_config = newconfig, exec_graph = (exec_graph int) ++ [(oldconfig, prog)]}
    where oldconfig = (expl_config int)
          newconfig = (expl_definterp int) prog oldconfig

displayEdge :: Show c => Show p => (c, p) ->  String
displayEdge (c1, p) = (show c1) ++ "\n" ++ "|\n" ++ "|\n| " ++ (show p) ++ "\n|\n|\nv\n"

display :: Show c => Show p => ExploringInt p c -> IO ()
display explorer = case (exec_graph explorer) of
    [] -> print (expl_config explorer)
    _ -> putStrLn $ concat (map displayEdge (exec_graph explorer)) ++ (show (expl_config explorer))


revert :: Eq c => ExploringInt p c -> c -> ExploringInt p c
revert explorer newconfig = explorer { expl_config = newconfig, exec_graph = newgraph }
    where newgraph = if length prunedGraph == length (exec_graph explorer) then [] else prunedGraph 
          prunedGraph = takeWhile (\(c, _) -> c /= newconfig) (exec_graph explorer)


type WhileExploringInt = ExploringInt Command Config

whileExplorer :: WhileExploringInt
whileExplorer = buildExplInt (definterp) (initialConfig)
