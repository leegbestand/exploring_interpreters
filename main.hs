import Whilelang
import Explorer
import System.IO

circleAssign :: Command
circleAssign = Seq (Seq (Assign "x" (intToExpr 1)) (Assign "x" (intToExpr 2))) (Assign "x" (intToExpr 1))

backTrackAssign :: Command
backTrackAssign = Seq (Assign "x" (intToExpr 1)) (Assign "x" (intToExpr 2))

backTrackAssign' :: Command
backTrackAssign' = Assign "x" (intToExpr 1)

outputTest :: Command
outputTest = wprint (intToExpr 10) `wseq` (wprint (intToExpr 100) `wseq` wprint (intToExpr 200))

structural :: WhileExplorer
structural = build Structural False definterp initialConfig

reference :: WhileExplorer
reference = build Reference False definterp initialConfig

backtracking :: WhileExplorer
backtracking = build Reference True definterp initialConfig

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


showBacktracking :: IO ()
showBacktracking = do
    explorer' <- repl backtracking backTrackAssign 
    let explorer'' = case revert explorer' 3 of
                    Just c  -> c
                    Nothing -> error "No valid ref" 
    explorer''' <- repl explorer'' backTrackAssign'
    displayDot explorer''
                    
-- With no backtracking, node 2 will have 2 children.
showNoBacktracking :: IO ()
showNoBacktracking = do
    explorer' <- repl reference backTrackAssign 
    let explorer'' = case revert explorer' 3 of
                    Just c  -> c
                    Nothing -> explorer'
    explorer''' <- repl explorer'' backTrackAssign'
    displayDot explorer''
