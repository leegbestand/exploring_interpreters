{-# LANGUAGE GADTs #-}

module Language.Explorer.Monadic
    ( Explorer
    , execute
    , executeAll
    , revert
    , toTree
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
    , getPathsFromTo
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
    Explorer :: (Show programs, Eq programs, Eq configs, Monad m, Monoid output) => 
        { sharing :: Bool
        , backTracking :: Bool
        , defInterp :: programs -> configs -> m (configs, output)
        , config :: configs -- Cache the config
        , currRef :: Ref
        , genRef :: Ref
        , cmap :: IntMap.IntMap configs
        -- TODO: Make Gr Ref programs
        , execEnv :: Gr Ref (programs, output)
        } -> Explorer programs m configs output

mkExplorer :: (Show a, Eq a, Eq b, Monad m, Monoid o) => 
  Bool -> Bool -> (a -> b -> m (b,o)) -> b -> Explorer a m b o
mkExplorer share backtrack definterp conf = Explorer 
    { defInterp = definterp
    , config = conf
    , genRef = 1 -- Currently generate references by increasing a counter.
    , currRef = initialRef
    , cmap = IntMap.fromList [(initialRef, conf)]
    , execEnv = mkGraph [(initialRef, initialRef)] []
    , sharing = share 
    , backTracking = backtrack
}

initialRef :: Int
initialRef = 1

mkExplorerStack, mkExplorerTree, mkExplorerGraph, mkExplorerGSS :: (Show a, Eq a, Eq b, Monad m, Monoid o) => (a -> b -> m (b,o)) -> b -> Explorer a m b o
mkExplorerStack = mkExplorer False True
mkExplorerTree  = mkExplorer False False
mkExplorerGraph = mkExplorer True False 
mkExplorerGSS   = mkExplorer True True

deref :: Explorer p m c o -> Ref -> Maybe c
deref e r = IntMap.lookup r (cmap e)

findRef :: Eq c => Explorer p m c o -> c -> Maybe (Ref, c)
findRef e c = find (\(r, c') -> c' == c) (IntMap.toList (cmap e))

addNewPath :: Explorer p m c o -> p -> o -> c -> Explorer p m c o
addNewPath e p o c = e { config = c, currRef = newref, genRef = newref, cmap = IntMap.insert newref c (cmap e),
     execEnv = insNode (newref, newref) $ insEdge (currRef e, newref, (p,o)) (execEnv e)}
     where newref = genRef e + 1

updateConf :: (Eq c, Eq p, Eq o) => Explorer p m c o -> (p, c, o) -> Explorer p m c o
updateConf e (p, newconf, output) = 
    if sharing e
        then case findRef e newconf of
            Just (r, c) -> 
                if hasLEdge (execEnv e) (currRef e, r, (p,output)) 
                    then e  { config = newconf, currRef = r }
                    else e  { config = newconf, currRef = r
                            , execEnv = insEdge (currRef e, r, (p,output)) (execEnv e) }
            Nothing -> addNewPath e p output newconf
        else addNewPath e p output newconf

execute :: (Eq c, Eq p, Eq o, Monad m, Monoid o) =>  p -> Explorer p m c o -> m (Explorer p m c o, o)
execute p e = do
    (newconf,out) <- defInterp e p (config e)
    return $ (updateConf e (p, newconf, out), out) 



executeAll :: (Eq c, Eq p, Eq o, Monad m, Monoid o) => [p] -> Explorer p m c o -> m (Explorer p m c o, o)
executeAll ps exp = foldlM executeCollect (exp, mempty) ps
  where executeCollect (exp, out) p = do (res, out') <- execute p exp
                                         return (res, out `mappend` out')

-- Implementation for execute with Monad, can replace execute if
-- monads are part of the explorer type.
{-- executeM :: (Eq c, Eq p) => p -> Explorer p m c -> IO (Explorer p m c)
executeM p e  = do
    c' <- defInterpM e p (config e)
    return $ updateConf e (p, c')
--}

    
deleteMap :: [Ref] -> IntMap.IntMap a -> IntMap.IntMap a
deleteMap xs m = foldl (flip IntMap.delete) m xs

revert :: Ref -> Explorer p m c o -> Maybe (Explorer p m c o)
revert r e = case IntMap.lookup r (cmap e) of
    Just c | backTracking e -> Just e { execEnv = execEnv', currRef = r, config = c, cmap = cmap'}
           | otherwise      -> Just e { currRef = r, config = c }
    Nothing                 -> Nothing
    where nodesToDel = reachable r (execEnv e) \\ [r]
          edgesToDel = filter (\(s, t) -> s `elem` nodesToDel || t `elem` nodesToDel) (edges (execEnv e))
          execEnv'   = (delEdges edgesToDel . delNodes nodesToDel) (execEnv e)
          cmap'      = deleteMap nodesToDel (cmap e)

toTree :: Explorer p m c o -> Tree (Ref, c)
toTree exp = mkTree initialRef
  where graph = execEnv exp 
        target (_, r, _) = r
        mkTree r = Node (r, cmap exp IntMap.! r) (map (mkTree . target) (out graph r))

subExecEnv :: Explorer p m c o -> Gr Ref (p,o)
subExecEnv e = subgraph (foldr (\(s, t) l ->  s : t : l) [] (filter (\(_, t) -> t == (currRef e)) (edges (execEnv e)))) (execEnv e)

transformToRealGraph :: Gr Ref p -> Gr Ref Int
transformToRealGraph g = mkGraph (labNodes g) (map (\(s, t) -> (s, t, 1)) (edges g))

transformToPairs :: [Ref] -> [(Ref, Ref)]
transformToPairs (s:t:xs) = (s, t) : transformToPairs (t:xs)
transformToPairs _ = []

getPathFromRootToCurr :: Explorer p m c o -> Gr Ref (p, o)
getPathFromRootToCurr e = mkGraph nl (filter ((\path -> \(s, t, _) -> (s, t) `elem` path) (transformToPairs n)) (labEdges $ execEnv e))
  where
    n = fromMaybe [] (sp 1 (currRef e) (transformToRealGraph (execEnv e)))
    nl = filter (\(i, _) -> i `elem` n) (labNodes (execEnv e))

mapOut :: Gr Ref (p,o) -> [Ref] -> Ref -> (Ref, Ref, (p,o)) -> Maybe [[(Ref, Ref, p, o)]]
mapOut gr visited goal (s, t, (l,o))
  | goal == t = Just $ [[(s, t, l, o)]] ++ explore
  | otherwise = case t `elem` visited of
          True  -> Nothing
          False -> Just explore
  where
    explore = map ((:)(s, t, l, o)) (concat $ catMaybes $ map (mapOut gr (t : visited) goal) (out gr t))
                                  


getPathsFromTo :: Ref -> Ref -> Explorer p m c o -> [[(Ref, Ref, p, o)]]
getPathsFromTo from to exp = concat $ catMaybes $ map (mapOut (execEnv exp) [from] to) (out (execEnv exp) from)

executionGraph :: Explorer p m c o -> (Ref, [Ref], [((Ref, c), (p, o), (Ref, c))])
executionGraph exp =
  (curr, nodes, edges)
  where
    curr = currRef exp
    nodes = map fst (labNodes (execEnv exp))
    edges = map (\(s, t, p) -> ((s, fromJust $ deref exp s), p, (t, fromJust $ deref exp t)) ) (labEdges (execEnv exp))

