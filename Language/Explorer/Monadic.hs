{-# LANGUAGE GADTs #-}

module Language.Explorer.Monadic
    ( Explorer
    , execute
    , executeAll
    , revert
    , dynamicRevert
    , toTree
    , incomingEdges
    , mkExplorerStack
    , mkExplorerTree
    , config
    , execEnv
    , currRef
    , leaves
    , Ref
    , deref
    , getTrace
    , getTraces
    , getPathsFromTo
    , getPathFromTo
    , executionGraph
    ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query
import Data.Graph.Inductive.Query.SP
import Data.Tree (Tree(..))

import qualified Data.IntMap as IntMap
import Data.List
import Data.Foldable
import Data.Maybe

type Ref = Int

data Explorer programs m configs output where
    Explorer :: (Monad m, Monoid output) =>
        { shadowing :: Bool -- Shadow the exploration tree in a shadow graph.
        , backTracking :: Bool
        , defInterp :: programs -> configs -> m (Maybe configs, output)
        , config :: configs -- Cache the config
        , currRef :: Ref
        , genRef :: Ref
        , cmap :: IntMap.IntMap configs
        , execEnv :: Gr Ref (programs, output)
        } -> Explorer programs m configs output


mkExplorer :: (Monad m, Monoid o) =>
  Bool -> Bool -> (a -> b -> m (Maybe b,o)) -> b -> Explorer a m b o
mkExplorer backtrack shadow definterp conf = Explorer
    { defInterp = definterp
    , config = conf
    , genRef = 1 -- Currently generate references by increasing a counter.
    , currRef = initialRef
    , cmap = IntMap.fromList [(initialRef, conf)]
    , execEnv = mkGraph [(initialRef, initialRef)] []
    , shadowing = shadow
    , backTracking = backtrack
}

initialRef :: Int
initialRef = 1

mkExplorerStack, mkExplorerTree :: (Monad m, Monoid o) => Bool -> (a -> b -> m (Maybe b,o)) -> b -> Explorer a m b o
mkExplorerStack = mkExplorer True
mkExplorerTree  = mkExplorer False

deref :: Explorer p m c o -> Ref -> Maybe c
deref e r = IntMap.lookup r (cmap e)

findRef :: Eq c => Explorer p m c o -> c -> Maybe (Ref, c)
findRef e c = find (\(r, c') -> c' == c) (IntMap.toList (cmap e))

addNewPath :: Explorer p m c o -> p -> o -> c -> Explorer p m c o
addNewPath e p o c = e { config = c, currRef = newref, genRef = newref, cmap = IntMap.insert newref c (cmap e),
     execEnv = insNode (newref, newref) $ insEdge (currRef e, newref, (p,o)) (execEnv e)}
     where newref = genRef e + 1

{--    if sharing e
        then case findRef e newconf of
            Just (r, c) ->
                if hasLEdge (execEnv e) (currRef e, r, (p,output))
                    then e  { config = newconf, currRef = r }
                    else e  { config = newconf, currRef = r
                            , execEnv = insEdge (currRef e, r, (p,output)) (execEnv e) }
            Nothing -> addNewPath e p output newconf
--}

updateConf :: Eq o => Explorer p m c o -> (p, c, o) -> Explorer p m c o
updateConf e (p, newconf, output) = addNewPath e p output newconf

execute :: (Eq o, Monad m, Monoid o) =>  p -> Explorer p m c o -> m (Explorer p m c o, o)
execute p e =
  do (mcfg, o) <- defInterp e p (config e)
     case mcfg of
       Just cfg -> return $ (updateConf e (p, cfg, o), o)
       Nothing  -> return (e, o)

executeAll :: (Eq o, Monad m, Monoid o) => [p] -> Explorer p m c o -> m (Explorer p m c o, o)
executeAll ps exp = foldlM executeCollect (exp, mempty) ps
  where executeCollect (exp, out) p = do (res, out') <- execute p exp
                                         return (res, out `mappend` out')

deleteMap :: [Ref] -> IntMap.IntMap a -> IntMap.IntMap a
deleteMap xs m = foldl (flip IntMap.delete) m xs

dynamicRevert :: Bool -> Ref -> Explorer p m c o -> Maybe (Explorer p m c o)
dynamicRevert backtrack r e =
  case IntMap.lookup r (cmap e) of
    Just c | backtrack -> Just e { execEnv = execEnv', currRef = r, config = c, cmap = cmap'}
           | otherwise -> Just e { currRef = r, config = c }
    Nothing            -> Nothing
    where nodesToDel = reachable r (execEnv e) \\ [r]
          edgesToDel = filter (\(s, t) -> s `elem` nodesToDel || t `elem` nodesToDel) (edges (execEnv e))
          execEnv'   = (delEdges edgesToDel . delNodes nodesToDel) (execEnv e)
          cmap'      = deleteMap nodesToDel (cmap e)


revert :: Ref -> Explorer p m c o -> Maybe (Explorer p m c o)
revert r e = dynamicRevert (backTracking e) r e


toTree :: Explorer p m c o -> Tree (Ref, c)
toTree exp = mkTree initialRef
  where graph = execEnv exp
        target (_, r, _) = r
        mkTree r = Node (r, cmap exp IntMap.! r) (map (mkTree . target) (out graph r))


incomingEdges :: Ref -> Explorer p m c o -> [((Ref, c), (p, o), (Ref, c))]
incomingEdges ref e = foldr (\(s, t, l) acc ->  [((s, unpack s), l, (t, unpack t))] ++ acc) [] (filter (\(_, t, _) -> t == ref) (labEdges (execEnv e)))
  where
    unpack ref = fromJust $ deref e ref


transformToRealGraph :: Gr Ref p -> Gr Ref Int
transformToRealGraph g = mkGraph (labNodes g) (map (\(s, t) -> (s, t, 1)) (edges g))

transformToPairs :: [Ref] -> [(Ref, Ref)]
transformToPairs (s:t:xs) = (s, t) : transformToPairs (t:xs)
transformToPairs _ = []

getTrace :: Explorer p m c o -> [((Ref, c), (p, o), (Ref, c))]
getTrace e = getPathFromTo e initialRef (currRef e)

getTraces :: Explorer p m c o -> [[((Ref, c), (p, o), (Ref, c))]]
getTraces e = getPathsFromTo e initialRef (currRef e)


mapOut :: Explorer p m c o -> Gr Ref (p, o) -> [Ref] -> Ref -> (Ref, Ref, (p,o)) -> Maybe [[((Ref, c), (p, o), (Ref, c))]]
mapOut exp gr visited goal (s, t, (l, o))
  | goal == t = Just $ [[((s, unpack s), (l, o), (t, unpack t))]] ++ explore
  | otherwise = case t `elem` visited of
                  True -> Nothing
                  False -> Just explore
  where
    explore = map ((:)((s, unpack s), (l, o), (t, unpack t))) (concat $ catMaybes $ map (mapOut exp gr (t : visited) goal) (out gr t))
    unpack ref = fromJust $ deref exp ref


getPathsFromTo :: Explorer p m c o -> Ref -> Ref -> [[((Ref, c), (p, o), (Ref, c))]]
getPathsFromTo exp from to = concat $ catMaybes $ map (mapOut exp (execEnv exp) [from] to) (out (execEnv exp) from)

getPathFromTo :: Explorer p m c o -> Ref -> Ref -> [((Ref, c), (p, o), (Ref, c))]
getPathFromTo exp from to =
  case getPathsFromTo exp from to of
    [] -> []
    (x:_) -> x



executionGraph :: Explorer p m c o -> (Ref, [Ref], [((Ref, c), (p, o), (Ref, c))])
executionGraph exp =
  (curr, nodes, edges)
  where
    curr = currRef exp
    nodes = map fst (labNodes (execEnv exp))
    edges = map (\(s, t, p) -> ((s, fromJust $ deref exp s), p, (t, fromJust $ deref exp t)) ) (labEdges (execEnv exp))

{-|
  Returns all configurations that have not been the source for an execute action.
  This corresponds to leaves in a tree or nodes without an outbound-edge in a graph.
-}
leaves :: Explorer p m c o -> [(Ref, c)]
leaves exp = map refToPair leave_nodes
  where
    env = execEnv exp
    refToPair = \r -> (r, fromJust $ deref exp r)
    leave_nodes = nodes $ nfilter (\n -> (==0) $ outdeg env n) env
