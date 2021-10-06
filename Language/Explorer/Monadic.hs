{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}


module Language.Explorer.Monadic
    ( Explorer
    , mkExplorer
    , execute
    , executeAll
    , revert
    , jump
    , toTree
    , incomingEdges
    , config
    , execEnv
    , currRef
    , leaves
    , Ref
    , Language
    , deref
    , getTrace
    , getTraces
    , getPathsFromTo
    , getPathFromTo
    , executionGraph
    , shadowExecEnv
    , eqClasses
    , initialRef
    , fromExport
    , toExport
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
type Language p m c o = (Eq p, Eq o, Monad m, Monoid o)

data Explorer programs m configs output where
    Explorer :: Language programs m configs output =>
        { shadowing :: Bool -- Shadow the exploration tree in a shadow graph.
        , defInterp :: programs -> configs -> m (Maybe configs, output)
        , config :: configs -- Cache the config
        , currRef :: Ref
        , genRef :: Ref
        , cmap :: IntMap.IntMap configs
        , execEnv :: Gr Ref (programs, output)
        , shadowExecEnv :: Gr [Ref] (programs, output)
        , configEq :: configs -> configs -> Bool
        } -> Explorer programs m configs output


mkExplorer :: Language p m c o => Bool -> (c -> c -> Bool) -> (p -> c -> m (Maybe c, o)) -> c -> Explorer p m c o
mkExplorer shadow shadowEq definterp conf = Explorer
    { defInterp = definterp
    , config = conf
    , genRef = 1 -- Currently generate references by increasing a counter.
    , currRef = initialRef
    , cmap = IntMap.fromList [(initialRef, conf)]
    , execEnv = mkGraph [(initialRef, initialRef)] []
    , shadowExecEnv = mkGraph [(initialRef, [initialRef])] []
    , shadowing = shadow
    , configEq = shadowEq
}

initialRef :: Int
initialRef = 1

mkExplorerNoSharing :: Language p m c o  => (p -> c -> m (Maybe c, o)) -> c -> Explorer p m c o
mkExplorerNoSharing = mkExplorer False (\_ -> \_ -> False)

deref :: Explorer p m c o -> Ref -> Maybe c
deref e r = IntMap.lookup r (cmap e)

findRef :: Explorer p m c o -> c -> (c -> Bool) -> Maybe (Ref, c)
findRef e c eq = find (\(r, c') -> eq c') (IntMap.toList (cmap e))

addNewPath :: Explorer p m c o -> p -> o -> c -> Explorer p m c o
addNewPath e p o c = e { config = c, currRef = newref, genRef = newref, cmap = IntMap.insert newref c (cmap e),
     execEnv = insNode (newref, newref) $ insEdge (currRef e, newref, (p,o)) (execEnv e)}
     where newref = genRef e + 1


findNodeRef :: Gr [Ref] (p, o) -> Ref -> Ref
findNodeRef g r = fst $ fromJust $ find (\(_, rs) -> r `elem` rs) $ labNodes g


updateShadowEnv :: Language p m c o => Explorer p m c o -> (p, c, o, Ref, Ref) -> Explorer p m c o
updateShadowEnv e (p, newconf, output, newref, oldref) =
  case findRef e newconf (configEq e newconf) of
    Just (r', _) ->
      if hasLEdge shadowEnv (nref, findNodeRef shadowEnv r', (p, output))
        then e
        else e {
          shadowExecEnv = updateLabel (findNodeRef shadowEnv r', newref, p, output) $ insEdge (nref, findNodeRef shadowEnv r', (p, output)) shadowEnv
        }
    Nothing -> e {
      shadowExecEnv = insNode (newref, [newref]) $ insEdge (nref, newref, (p, output)) $ shadowExecEnv e
    }
    where
      shadowEnv = shadowExecEnv e
      nref = findNodeRef shadowEnv oldref
      updateLabel (target, add_to_label, p, output) gr =
        case match target gr of
          (Just (toadj, node, label, fromadj), decomgr) -> (toadj, node, add_to_label : label, fromadj) & decomgr
          _ -> error "Shadow execution environment is inconsistent."


updateExecEnvs :: Language p m c o => Explorer p m c o -> (p, c, o) -> Explorer p m c o
updateExecEnvs e (p, newconf, output)
  | shadowing e = addNewPath (updateShadowEnv e (p, newconf, output, (currRef newexplorer), (currRef e))) p output newconf
  | otherwise = newexplorer
  where
    newexplorer = addNewPath e p output newconf

execute :: Language p m c o =>  p -> Explorer p m c o -> m (Explorer p m c o, o)
execute p e =
  do (mcfg, o) <- defInterp e p (config e)
     case mcfg of
       Just cfg -> return (updateExecEnvs e (p, cfg, o), o)
       Nothing  -> return (e, o)

executeAll :: Language p m c o => [p] -> Explorer p m c o -> m (Explorer p m c o, o)
executeAll ps exp = foldlM executeCollect (exp, mempty) ps
  where executeCollect (exp, out) p = do (res, out') <- execute p exp
                                         return (res, out `mappend` out')

deleteMap :: [Ref] -> IntMap.IntMap a -> IntMap.IntMap a
deleteMap xs m = foldl (flip IntMap.delete) m xs


deleteFromShadowEnv :: [(Ref, Ref)] -> Gr [Ref] po -> Gr [Ref] po
deleteFromShadowEnv [(l, r)] gr = case match r gr of
  (Just (toadj, node, label, fromadj), decomgr) -> (toadj, node, label \\ [l], fromadj) & decomgr
  _ -> error "Inconsistent shadow env."
deleteFromShadowEnv (x:xs) gr = deleteFromShadowEnv xs (deleteFromShadowEnv [x] gr)

cleanEdges :: [Ref] -> [(Ref, Ref, (p, o))] -> [(Ref, Ref, (p, o))]
cleanEdges ref edg = filter (\(s, t, l) -> not $ t `elem` ref) edg

cleanShadowEnv :: Bool -> [Ref] -> Gr [Ref] (p, o) -> Gr [Ref] (p, o)
cleanShadowEnv False _ g = g
cleanShadowEnv True nds g = shadowEnv''
  where
    shadowEnv' = deleteFromShadowEnv (map (\r -> (r, findNodeRef g r)) nds) g
    nodesToDel' = map fst (filter (\(r, labels) -> labels == []) $ labNodes (shadowEnv'))
    edgesToDel' = filter (\(s, t) -> s `elem` nodesToDel' || t `elem` nodesToDel') (edges shadowEnv')
    shadowEnv'' = (delEdges edgesToDel' . delNodes nodesToDel') shadowEnv'

data RevertableStatus = ContinueRevert | StopRevert deriving Show

findRevertableNodes gr source target = 
  case findNextNodeInPath gr source target of
    (Just node) -> fst $ findRevertableNodes' gr node target
    Nothing     -> []
  where 
    findNextNodeInPath gr source target = find (\n -> target `elem` (reachable n gr)) (suc gr source) 
    findRevertableNodes' gr source target 
      | source == target = if outdeg gr source > 1 then ([], StopRevert) else ([source], ContinueRevert) 
      | otherwise = case findNextNodeInPath gr source target of
        (Just node) -> case findRevertableNodes' gr node target of
          (res, StopRevert) -> (res, StopRevert)
          (res, ContinueRevert) -> if outdeg gr source > 1 then (res, StopRevert) else (source : res, ContinueRevert)
        Nothing -> ([], ContinueRevert)

revert :: Ref -> Explorer p m c o -> Maybe (Explorer p m c o)
revert r e
  | currRef e `elem` reachNodes =
      jump r e >>= \e' -> return $ e' { execEnv = mkGraph (zip remainNodes remainNodes) $ cleanEdges reachNodes (labEdges $ execEnv e')
                                      ,  cmap = deleteMap reachNodes (cmap e')
                                      , shadowExecEnv = cleanShadowEnv (shadowing e') reachNodes (shadowExecEnv e')}
  | otherwise = Nothing
  where
    reachNodes = findRevertableNodes gr r (currRef e)
    remainNodes = nodes gr \\ reachNodes
    gr = execEnv e

jump :: Ref -> Explorer p m c o -> Maybe (Explorer p m c o)
jump r e = case deref e r of
             (Just c) -> return $ e { config = c, currRef = r }
             Nothing -> Nothing

toTree :: Explorer p m c o -> Tree (Ref, c)
toTree exp = mkTree initialRef
  where graph = execEnv exp
        target (_, r, _) = r
        mkTree r = Node (r, cmap exp IntMap.! r) (map (mkTree . target) (out graph r))


incomingEdges :: Ref -> Explorer p m c o -> [((Ref, c), (p, o), (Ref, c))]
incomingEdges ref e = foldr (\(s, t, l) acc ->  [((s, unpack s), l, (t, unpack t))] ++ acc) [] (filter (\(_, t, _) -> t == ref) (labEdges (execEnv e)))
  where
    unpack ref = fromJust $ deref e ref

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



executionGraph :: Explorer p m c o -> ((Ref, c), [(Ref, c)], [((Ref, c), (p, o), (Ref, c))])
executionGraph exp =
  (curr, nodes, edges)
  where
    curr = (currRef exp, config exp)
    nodes = IntMap.toList $ cmap exp
    edges = map (\(s, t, p) -> ((s, fromJust $ deref exp s), p, (t, fromJust $ deref exp t)) ) (labEdges (execEnv exp))

{-|
  Returns all configurations that have not been the source for an execute action.
  This corresponds to leaves in a tree or nodes without an outbound-edge in a graph.
-}
leaves :: Explorer p m c o -> [(Ref, c)]
leaves exp = map refToPair leaf_nodes
  where
    env = execEnv exp
    refToPair = \r -> (r, fromJust $ deref exp r)
    leaf_nodes = nodes $ nfilter (\n -> (==0) $ outdeg env n) env


toExport :: Explorer p m c o -> (Ref, [(Ref, c)], [(Ref, Ref, (p, o))])
toExport exp = (currRef exp, IntMap.toList $ cmap exp, labEdges $ execEnv exp)

fromExport :: Explorer p m c o -> (Ref, [(Ref, c)], [(Ref, Ref, (p, o))]) -> Explorer p m c o
fromExport exp (curr, nds, edgs) = exp { genRef = findMax nds,
                                         config = findCurrentConf curr nds,
                                         currRef = curr, 
                                         cmap = IntMap.fromList nds, 
                                         execEnv = mkGraph (map (\(x, _) -> (x, x)) nds) edgs }
  where findMax l = maximum $ map fst l
        findCurrentConf curr nds = case lookup curr nds of
                                     Just conf -> conf
                                     Nothing   -> error "no config found"

eqClasses :: Explorer p m c o -> [[Ref]]
eqClasses expl = map snd $ labNodes $ shadowExecEnv expl
