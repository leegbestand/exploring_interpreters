{-# LANGUAGE ConstraintKinds #-}

module Language.Explorer.Basic
    ( Explorer
    , mkExplorer
    , mkExplorerNoSharing
    , execute
    , executeAll
    , revert
    , ExplorerM.toTree
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
import Data.Functor
import Data.Foldable
import Data.Monoid ()
import Data.Graph.Inductive.Graph (emap)

-- We shadow instead of exporting directly to make the user interaction
-- the same.
type Ref = ExplorerM.Ref
type Explorer a b = ExplorerM.Explorer a Identity b ()
type BasicLanguage p c = Eq p

mkExplorer :: BasicLanguage p c => Bool -> (c -> c -> Bool) -> (p -> c -> Maybe c) -> c -> Explorer p c
mkExplorer shadow eqfunc definterp initialConf = ExplorerM.mkExplorer shadow eqfunc (wrap definterp) initialConf

mkExplorerNoSharing :: BasicLanguage p c => (p -> c -> Maybe c) -> c -> Explorer p c
mkExplorerNoSharing = mkExplorer False (const . const $ False)

currRef :: Explorer a b -> Ref
currRef = ExplorerM.currRef

config :: Explorer a b -> b
config = ExplorerM.config

deref :: Explorer p c -> Ref -> Maybe c
deref = ExplorerM.deref

-- This should be able with func composition.
wrap :: Monad m => (a -> b -> Maybe b) -> a -> b -> m (Maybe b, ())
wrap def p e = return $ (def p e, ())

execute :: BasicLanguage p c => p -> Explorer p c -> Explorer p c
execute p e = fst $ runIdentity $ ExplorerM.execute p e

executeAll :: BasicLanguage p c => [p] -> Explorer p c -> Explorer p c
executeAll p e = fst $ runIdentity $ ExplorerM.executeAll p e

revert :: ExplorerM.Ref -> Explorer p c -> Maybe (Explorer p c)
revert = ExplorerM.revert

removeOutput :: ((Ref, c), (p, o), (Ref, c)) -> ((Ref, c), p, (Ref, c))
removeOutput (s, (p, _), t) = (s, p, t)

incomingEdges :: Ref -> Explorer p c -> [((Ref, c), p, (Ref, c))]
incomingEdges r e = map removeOutput $ ExplorerM.incomingEdges r e

getTrace :: Explorer p c -> [((Ref, c), p, (Ref, c))]
getTrace e = map removeOutput $ ExplorerM.getTrace e

getTraces :: Explorer p c -> [[((Ref, c), p, (Ref, c))]]
getTraces e = map (map removeOutput) $ ExplorerM.getTraces e

getPathsFromTo :: Explorer p c -> Ref -> Ref -> [[((Ref, c), p, (Ref, c))]]
getPathsFromTo e s t = map (map removeOutput) $ ExplorerM.getPathsFromTo e s t

getPathFromTo :: Explorer p c -> Ref -> Ref -> [((Ref, c), p, (Ref, c))]
getPathFromTo e s t = map removeOutput $ ExplorerM.getPathFromTo e s t

executionGraph :: Explorer p c -> ((Ref, c), [(Ref, c)], [((Ref, c), p, (Ref, c))])
executionGraph e = (curr, nodes, map removeOutput graph)
  where
    (curr, nodes, graph) = ExplorerM.executionGraph e

leaves :: Explorer p c -> [(Ref, c)]
leaves = ExplorerM.leaves

toExport :: Explorer p c -> (Ref, [(Ref, c)], [(Ref, Ref, p)])
toExport = removeOut . ExplorerM.toExport
  where 
    removeOut (c, nodes, edges) = (c, nodes, map (\(s, t, (p, _)) -> (s, t, p)) edges)

fromExport :: Explorer p c -> (Ref, [(Ref, c)], [(Ref, Ref, p)]) -> Explorer p c
fromExport e exported = ExplorerM.fromExport e (addOut exported)
  where
    addOut (c, nodes, edges) = (c, nodes, map (\(s, t, p) -> (s, t, (p, ()))) edges)