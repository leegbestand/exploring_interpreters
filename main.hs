import Whilelang
import Explorer
import System.IO

circleAssign :: Command
circleAssign = Seq (Seq (Assign "x" (intToExpr 1)) (Assign "x" (intToExpr 2))) (Assign "x" (intToExpr 1))

backTrackAssign :: Command
backTrackAssign = Seq (Assign "x" (intToExpr 1)) (Assign "x" (intToExpr 2))

backTrackAssign' :: Command
backTrackAssign' = Assign "x" (intToExpr 2)

outputTest :: Command
outputTest = wprint (intToExpr 10) `wseq` (wprint (intToExpr 100) `wseq` wprint (intToExpr 200))

structural :: WhileExplorer
structural = build Structural False definterp initialConfig

reference :: WhileExplorer
reference = build Reference False definterp initialConfig

backtracking :: WhileExplorer
backtracking = build Reference True definterp initialConfig

displayDot' :: WhileExplorer -> String -> IO ()
displayDot' e filename = do
    handle <- openFile filename WriteMode
    writeFile 

showStructural :: IO ()
showStructural = do 
    e' <- repl structural circleAssign
    displayDot e'

showReference :: IO ()
showReference = do
    e' <- repl reference circleAssign
    displayDot e'

-- Shows how the REPL would extract a Seq into multiple execute calls
-- and display the output of every call.
showOutput :: IO ()
showOutput = do 
    e' <- repl structural outputTest
    displayDot e'


-- Backtracking will destroy node 3 and introduce on the second repl call node 4.
showBacktracking :: IO ()
showBacktracking = do
    explorer' <- repl backtracking backTrackAssign 
    explorer'' <- repl (revert explorer' 2) backTrackAssign'
    displayDot explorer''
                    
-- When backtracking is disabled, node 3 is not destroyed and can be re-used.
-- So, no node 4.
showNoBacktracking :: IO ()
showNoBacktracking = do
    explorer' <- repl reference backTrackAssign 
    explorer'' <- repl (revert explorer' 2) backTrackAssign'
    displayDot explorer''
