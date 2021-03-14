module Language.Explorer.Basic
    ( Explorer
    , execute
    , executeAll
    , revert
    , ExplorerM.toTree
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
import Data.Functor
import Data.Foldable
import Data.Monoid ()
import Data.Graph.Inductive.Graph (emap)

-- We shadow instead of exporting directly to make the user interaction
-- the same.
type Ref = ExplorerM.Ref
type Explorer a b = ExplorerM.Explorer a Identity b ()

currRef :: Explorer a b -> Ref
currRef = ExplorerM.currRef

config :: Explorer a b -> b
config = ExplorerM.config

deref :: Explorer p c -> Ref -> Maybe c
deref = ExplorerM.deref

-- This should be able with func composition.
wrap :: Monad m => (a -> b -> b) -> a -> b -> m (b, ())
wrap def p e = return $ (def p e, ())

-- Constructor for a exploring interpreter.
mkExplorerStack:: (Show a, Eq a, Eq b) => (a -> b -> b) -> b -> Explorer a b
mkExplorerStack definterp conf = ExplorerM.mkExplorerStack (wrap definterp) conf

mkExplorerTree:: (Show a, Eq a, Eq b) => (a -> b -> b) -> b -> Explorer a b
mkExplorerTree definterp conf = ExplorerM.mkExplorerTree (wrap definterp) conf

mkExplorerGraph :: (Show a, Eq a, Eq b) => (a -> b -> b) -> b -> Explorer a b
mkExplorerGraph definterp conf = ExplorerM.mkExplorerGraph (wrap definterp) conf

mkExplorerGSS :: (Show a, Eq a, Eq b) => (a -> b -> b) -> b -> Explorer a b
mkExplorerGSS definterp conf = ExplorerM.mkExplorerGSS (wrap definterp) conf

execute :: (Eq c, Eq p) =>  p -> Explorer p c -> Explorer p c
execute p e = fst $ runIdentity $ ExplorerM.execute p e

executeAll :: (Eq c, Eq p) => [p] -> Explorer p c -> Explorer p c
executeAll p e = fst $ runIdentity $ ExplorerM.executeAll p e

revert :: ExplorerM.Ref -> Explorer p c -> Maybe (Explorer p c)
revert = ExplorerM.revert

removeOutput :: ((Ref, c), (p, o), (Ref, c)) -> ((Ref, c), p, (Ref, c))
removeOutput (s, (p, _), t) = (s, p, t)

incomingEdges :: Ref -> Explorer p c -> [((Ref, c), p, (Ref, c))]
incomingEdges r e = map removeOutput $ ExplorerM.incomingEdges r e

pathFromRootToCurr :: Explorer p c -> Maybe [((Ref, c), p, (Ref, c))]
pathFromRootToCurr e = map removeOutput <$> ExplorerM.pathFromRootToCurr e

pathsFromRootToCurr :: Explorer p c -> [[((Ref, c), p, (Ref, c))]]
pathsFromRootToCurr e = map (map removeOutput) $ ExplorerM.pathsFromRootToCurr e

pathsFromTo :: Ref -> Ref -> Explorer p c -> [[((Ref, c), p, (Ref, c))]]
pathsFromTo s t e = map (map removeOutput) $ ExplorerM.pathsFromTo s t e

pathFromTo :: Ref -> Ref -> Explorer p c -> Maybe [((Ref, c), p, (Ref, c))]
pathFromTo s t e = map removeOutput <$> ExplorerM.pathFromTo s t e

executionGraph :: Explorer p c -> (Ref, [Ref], [((Ref, c), p, (Ref, c))])
executionGraph e = (curr, nodes, map removeOutput graph)
  where
    (curr, nodes, graph) = ExplorerM.executionGraph e
