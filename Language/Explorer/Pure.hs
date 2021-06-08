module Language.Explorer.Pure
    ( Explorer
    , execute
    , executeAll
    , revert
    , dynamicRevert
    , ExplorerM.toTree
    , incomingEdges
    , mkExplorerStack
    , mkExplorerTree
    , mkExplorerGraph
    , mkExplorerGSS
    , config
    , currRef
    , Ref
    , ExplorerM.initialRef
    , deref
    , leaves
    , getTrace
    , getTraces
    , getPathsFromTo
    , getPathFromTo
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

wrap :: (Monad m, Monoid o) => (a -> b -> (Maybe b,o)) -> a -> b -> m (Maybe b, o)
wrap def p e = return $ def p e

mkExplorerStack, mkExplorerTree, mkExplorerGraph, mkExplorerGSS:: (Show a, Eq a, Eq b, Monoid o) => (a -> b -> (Maybe b,o)) -> b -> Explorer a b o
mkExplorerStack definterp conf = ExplorerM.mkExplorerStack (wrap definterp) conf
mkExplorerTree definterp conf = ExplorerM.mkExplorerTree (wrap definterp) conf
mkExplorerGraph definterp conf = ExplorerM.mkExplorerGraph (wrap definterp) conf
mkExplorerGSS definterp conf = ExplorerM.mkExplorerGSS (wrap definterp) conf

execute :: (Eq c, Eq p, Eq o, Monoid o) =>  p -> Explorer p c o -> (Explorer p c o, o)
execute p e = runIdentity $ ExplorerM.execute p e

executeAll :: (Eq c, Eq p, Eq o, Monoid o) => [p] -> Explorer p c o -> (Explorer p c o, o)
executeAll p e = runIdentity $ ExplorerM.executeAll p e

dynamicRevert :: Bool -> Ref -> Explorer p c o -> Maybe (Explorer p c o)
dynamicRevert = ExplorerM.dynamicRevert

revert :: ExplorerM.Ref -> Explorer p c o -> Maybe (Explorer p c o)
revert = ExplorerM.revert

incomingEdges :: Ref -> Explorer p c o -> [((Ref, c), (p, o), (Ref, c))]
incomingEdges = ExplorerM.incomingEdges

getTrace :: Explorer p c o -> [((Ref, c), (p, o), (Ref, c))]
getTrace = ExplorerM.getTrace

getTraces :: Explorer p c o -> [[((Ref, c), (p, o), (Ref, c))]]
getTraces = ExplorerM.getTraces

getPathsFromTo :: Explorer p c o -> Ref -> Ref -> [[((Ref, c), (p, o), (Ref, c))]]
getPathsFromTo = ExplorerM.getPathsFromTo

getPathFromTo :: Explorer p c o -> Ref -> Ref -> [((Ref, c), (p, o), (Ref, c))]
getPathFromTo = ExplorerM.getPathFromTo

executionGraph :: Explorer p c o -> (Ref, [Ref], [((Ref, c), (p, o), (Ref, c))])
executionGraph = ExplorerM.executionGraph

leaves :: Explorer p c o -> [(Ref, c)]
leaves = ExplorerM.leaves
