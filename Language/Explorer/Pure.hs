{-# LANGUAGE ConstraintKinds #-}

module Language.Explorer.Pure
    ( Explorer
    , mkExplorer
    , mkExplorerNoSharing
    , execute
    , executeAll
    , revert
    , ExplorerM.toTree
    , incomingEdges
    , config
    , currRef
    , Ref
    , deref
    , leaves
    , getTrace
    , getTraces
    , getPathsFromTo
    , getPathFromTo
    , executionGraph
    , fromExport
    , toExport
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
type PureLanguage p c o = (Eq p, Eq o, Monoid o)

mkExplorer :: PureLanguage p c o => Bool -> (c -> c -> Bool) -> (p -> c -> (Maybe c, o)) -> c -> Explorer p c o
mkExplorer shadowing eqfunc definterp initialConf = ExplorerM.mkExplorer shadowing eqfunc (wrap definterp) initialConf

mkExplorerNoSharing :: PureLanguage p c o => (p -> c -> (Maybe c, o)) -> c -> Explorer p c o
mkExplorerNoSharing = mkExplorer False (const . const $ False)

currRef :: Explorer a b o -> Ref
currRef = ExplorerM.currRef

config :: Explorer a b o -> b
config = ExplorerM.config

deref :: Explorer p c o -> Ref -> Maybe c
deref = ExplorerM.deref

wrap :: (Monad m, Monoid o) => (a -> b -> (Maybe b, o)) -> a -> b -> m (Maybe b, o)
wrap def p e = return $ def p e

execute :: PureLanguage p c o =>  p -> Explorer p c o -> (Explorer p c o, o)
execute p e = runIdentity $ ExplorerM.execute p e

executeAll :: PureLanguage p c o => [p] -> Explorer p c o -> (Explorer p c o, o)
executeAll p e = runIdentity $ ExplorerM.executeAll p e

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

executionGraph :: Explorer p c o -> ((Ref, c), [(Ref, c)], [((Ref, c), (p, o), (Ref, c))])
executionGraph = ExplorerM.executionGraph

leaves :: Explorer p c o -> [(Ref, c)]
leaves = ExplorerM.leaves

toExport :: Explorer p c o -> (Ref, [(Ref, c)], [(Ref, Ref, (p, o))])
toExport = ExplorerM.toExport

fromExport :: Explorer p c o -> (Ref, [(Ref, c)], [(Ref, Ref, (p, o))]) -> Explorer p c o
fromExport = ExplorerM.fromExport