
module Language.Explorer.Pure
    ( Explorer
    , execute
    , executeAll
    , revert
    , displayDot
    , display
    , displayExecEnv
    , ExplorerM.toTree
    , subExecEnv
    , mkExplorerStack
    , mkExplorerTree
    , mkExplorerGraph
    , mkExplorerGSS
    , config
    , currRef
    , Ref
    , deref
    , execEnv
    , Gr
    , getPathFromRootToCurr
    ) where

import qualified Language.Explorer.Monadic as ExplorerM
import Control.Monad.Identity

import qualified Data.IntMap as IntMap
import Data.List
import Data.Foldable

-- We shadow instead of exporting directly to make the user interaction
-- the same.
type Ref = ExplorerM.Ref
type Gr = ExplorerM.Gr
type Explorer a b o = ExplorerM.Explorer a Identity b o

currRef :: Explorer a b o -> Ref
currRef = ExplorerM.currRef

execEnv :: Explorer a b o -> Gr Ref (a, o)
execEnv = ExplorerM.execEnv

config :: Explorer a b o -> b
config = ExplorerM.config

deref :: Explorer p c o -> Ref -> Maybe c
deref = ExplorerM.deref

-- This should be able with func composition.
wrap :: (Monad m, Monoid o) => (a -> b -> (b,o)) -> a -> b -> m (b, o)
wrap def p e = return $ def p e

-- TODO: Put types into variables(if possible?).
-- Constructor for a exploring interpreter.
mkExplorerStack, mkExplorerTree, mkExplorerGraph, mkExplorerGSS:: (Show a, Eq a, Eq b, Monoid o) => (a -> b -> (b,o)) -> b -> Explorer a b o 
mkExplorerStack definterp conf = ExplorerM.mkExplorerStack (wrap definterp) conf
mkExplorerTree definterp conf = ExplorerM.mkExplorerTree (wrap definterp) conf
mkExplorerGraph definterp conf = ExplorerM.mkExplorerGraph (wrap definterp) conf
mkExplorerGSS definterp conf = ExplorerM.mkExplorerGSS (wrap definterp) conf

execute :: (Eq c, Eq p, Eq o, Monoid o) =>  p -> Explorer p c o -> (Explorer p c o, o)
execute p e = runIdentity $ ExplorerM.execute p e

executeAll :: (Eq c, Eq p, Eq o, Monoid o) => [p] -> Explorer p c o -> (Explorer p c o, o)
executeAll p e = runIdentity $ ExplorerM.executeAll p e

revert :: ExplorerM.Ref -> Explorer p c o -> Maybe (Explorer p c o)
revert = ExplorerM.revert

display :: (Show p, Show o) => Explorer p c o -> String
display = ExplorerM.display

displayDot :: (Show p, Show o) => Explorer p c o  -> IO ()
displayDot = ExplorerM.displayDot

displayExecEnv :: (Show p, Show o) => Gr Ref (p,o) -> String
displayExecEnv = ExplorerM.displayExecEnv

subExecEnv :: Explorer p c o -> Gr Ref (p,o)
subExecEnv = ExplorerM.subExecEnv

getPathFromRootToCurr :: Explorer p c o -> Gr Ref (p, o)
getPathFromRootToCurr = ExplorerM.getPathFromRootToCurr
