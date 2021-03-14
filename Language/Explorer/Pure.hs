module Language.Explorer.Pure
    ( Explorer
    , execute
    , executeAll
    , revert
    , ExplorerM.toTree
    , incomingEdges
    , mkExplorerStack
    , mkExplorerTree
    , mkExplorerGraph
    , mkExplorerGSS
    , config
    , currRef
    , Ref
    , deref
    , pathFromRootToCurr
    , pathsFromRootToCurr
    , pathsFromTo
    , pathFromTo
    , executionGraph
    ) where

import qualified Language.Explorer.Monadic as ExplorerM
import Control.Monad.Identity

import qualified Data.IntMap as IntMap
import Data.List
import Data.Foldable

-- We shadow instead of exporting directly to make the user interaction
-- the same.
type Ref = ExplorerM.Ref
type Explorer a b o = ExplorerM.Explorer a Identity b o

currRef :: Explorer a b o -> Ref
currRef = ExplorerM.currRef

config :: Explorer a b o -> b
config = ExplorerM.config

deref :: Explorer p c o -> Ref -> Maybe c
deref = ExplorerM.deref

wrap :: (Monad m, Monoid o) => (a -> b -> (b,o)) -> a -> b -> m (b, o)
wrap def p e = return $ def p e

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

incomingEdges :: Ref -> Explorer p c o -> [((Ref, c), (p, o), (Ref, c))]
incomingEdges = ExplorerM.incomingEdges

pathFromRootToCurr :: Explorer p c o -> Maybe [((Ref, c), (p, o), (Ref, c))]
pathFromRootToCurr = ExplorerM.pathFromRootToCurr

pathsFromRootToCurr :: Explorer p c o -> [[((Ref, c), (p, o), (Ref, c))]]
pathsFromRootToCurr = ExplorerM.pathsFromRootToCurr

pathsFromTo :: Ref -> Ref -> Explorer p c o -> [[((Ref, c), (p, o), (Ref, c))]]
pathsFromTo = ExplorerM.pathsFromTo

pathFromTo :: Ref -> Ref -> Explorer p c o -> Maybe [((Ref, c), (p, o), (Ref, c))]
pathFromTo = ExplorerM.pathFromTo

executionGraph :: Explorer p c o -> (Ref, [Ref], [((Ref, c), (p, o), (Ref, c))])
executionGraph = ExplorerM.executionGraph
