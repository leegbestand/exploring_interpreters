{-# LANGUAGE GADTs #-}

module ExploringInterpreterM
    ( Explorer
    , execute
    , executeAll
    , revert
    , displayDot
    , display
    , displayExecEnv
    , toTree
    , subExecEnv
    , mkExplorerStack
    , mkExplorerTree
    , mkExplorerGraph
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

data Explorer programs m configs where
    Explorer :: (Show programs, Eq programs, Eq configs, Monad m) => 
        { sharing :: Bool
        , backTracking :: Bool
        , defInterp :: programs -> configs -> m configs
        , config :: configs -- Cache the config
        , currRef :: Ref
        , genRef :: Ref
        , cmap :: IntMap.IntMap configs
        -- TODO: Make Gr Ref programs
        , execEnv :: Gr () programs
        } -> Explorer programs m configs

-- TODO: Put types into variables(if possible?).
-- Constructor for a exploring interpreter.
mkExplorer :: (Show a, Eq a, Eq b, Monad m) => Bool -> Bool -> (a -> b -> m b) -> b -> Explorer a m b
mkExplorer share backtrack definterp conf = Explorer 
    { defInterp = definterp
    , config = conf
    , genRef = 1 -- Currently generate reference by increasing a counter.
    , currRef = initialRef
    , cmap = IntMap.fromList [(1, conf)]
    , execEnv = mkGraph [(1, ())] []
    , sharing = share 
    , backTracking = backtrack
}

initialRef :: Int
initialRef = 1

mkExplorerStack :: (Show a, Eq a, Eq b, Monad m) => (a -> b -> m b) -> b -> Explorer a m b
mkExplorerStack = mkExplorer False True

mkExplorerTree  :: (Show a, Eq a, Eq b, Monad m) => (a -> b -> m b) -> b -> Explorer a m b 
mkExplorerTree  = mkExplorer False False

mkExplorerGraph :: (Show a, Eq a, Eq b, Monad m) => (a -> b -> m b) -> b -> Explorer a m b
mkExplorerGraph = mkExplorer True False 

deref :: Explorer p m c -> Ref -> Maybe c
deref e r = IntMap.lookup r (cmap e)

findRef :: Eq c => Explorer p m c -> c -> Maybe (Ref, c)
findRef e c = find (\(r, c') -> c' == c) (IntMap.toList (cmap e))

addNewPath :: Explorer p m c -> p -> c -> Explorer p m c
addNewPath e p c = e { config = c, currRef = newref, genRef = newref, cmap = IntMap.insert newref c (cmap e),
     execEnv = insNode (newref, ()) $ insEdge (currRef e, newref, p) (execEnv e)}
     where newref = genRef e + 1

-- Check if we already have an edge from the current node, if not create a new one.
handleRef :: Eq p => Explorer p m c -> p -> (Ref, c) -> Explorer p m c
handleRef e p (r, c) = if (currRef e, r, p) `elem` out (execEnv e) (currRef e)
    then e { currRef = r, config = c }
    else addNewPath e p c

updateConf :: Eq c => Eq p => Explorer p m c -> (p, c) -> Explorer p m c
updateConf e (p, newconf) = 
    if sharing e
        then case findRef e newconf of
            Just (r, c) -> 
                if hasLEdge (execEnv e) (currRef e, r, p) 
                    then e  { config = newconf, currRef = r }
                    else e  { config = newconf, currRef = r
                            , execEnv = insEdge (currRef e, r, p) (execEnv e) }
            Nothing -> addNewPath e p newconf
        else addNewPath e p newconf

execute :: (Eq c, Eq p, Monad m) =>  p -> Explorer p m c -> m (Explorer p m c)
execute p e = do
    newconf <- defInterp e p (config e)
    return $ updateConf e (p, newconf) 



executeAll :: (Eq c, Eq p, Monad m) => [p] -> Explorer p m c -> m (Explorer p m c)
executeAll = flip (foldlM $ flip execute)

-- Implementation for execute with Monad, can replace execute if
-- monads are part of the explorer type.
{-- executeM :: (Eq c, Eq p) => p -> Explorer p m c -> IO (Explorer p m c)
executeM p e  = do
    c' <- defInterpM e p (config e)
    return $ updateConf e (p, c')
--}

    
deleteMap :: [Ref] -> IntMap.IntMap a -> IntMap.IntMap a
deleteMap xs m = foldl (flip IntMap.delete) m xs

revert :: Ref -> Explorer p m c -> Maybe (Explorer p m c)
revert r e = case IntMap.lookup r (cmap e) of
    Just c | backTracking e -> Just e { execEnv = execEnv', currRef = r, config = c, cmap = cmap'}
           | otherwise      -> Just e { currRef = r, config = c }
    Nothing                 -> Nothing
    where nodesToDel = reachable r (execEnv e) \\ [r]
          edgesToDel = filter (\(s, t) -> s `elem` nodesToDel || t `elem` nodesToDel) (edges (execEnv e))
          execEnv'   = (delEdges edgesToDel . delNodes nodesToDel) (execEnv e)
          cmap'      = deleteMap nodesToDel (cmap e)


displayDotEdge :: Show v => Show e => (v, v, e) -> IO ()
displayDotEdge (s, t, e) = do
    putStr (show s)
    putStr " -> "
    putStr (show t)
    putStr "[label="
    putStr $ show (show e)
    putStrLn "]"

displayDotVertices :: Ref -> Node -> IO ()
displayDotVertices r n = if n == r then putStrLn ((show n) ++ " [shape=box]") else putStrLn (show n)
    

displayDot :: Show p => Explorer p m c  -> IO ()
displayDot g = do
    putStrLn "digraph g {"
    mapM_ (displayDotVertices (currRef g)) (nodes (execEnv g))
    mapM_ displayDotEdge (labEdges (execEnv g))
    putStrLn "}"

-- TODO: define in terms of displayGr
display :: Show p => Explorer p m c -> String
display e = "{\n\"edges\": \"" ++ show (labEdges (execEnv e)) ++ "\",\n"
          ++ "\"vertices\": \"" ++ show (nodes (execEnv e)) ++ "\",\n"
          ++ "\"current\": \"" ++ (show (currRef e)) ++ "\"\n"
          ++ "}"

displayExecEnv :: Show p => Gr () p -> String
displayExecEnv gr = "{\n\"edges\": \"" ++ show (labEdges gr) ++ "\",\n"
                  ++ "\"vertices\": \"" ++ show (nodes gr) ++ "\",\n"
                  ++ "}"

toTree :: Explorer p m c -> Tree (Ref, c)
toTree exp = mkTree initialRef
  where graph = execEnv exp 
        target (_, r, _) = r
        mkTree r = Node (r, cmap exp IntMap.! r) (map (mkTree . target) (out graph r))

subExecEnv :: Explorer p m c -> Gr () p
subExecEnv e = subgraph (foldr (\(s, t) l ->  s : t : l) [] (filter (\(_, t) -> t == (currRef e)) (edges (execEnv e)))) (execEnv e)


transformToRealGraph :: Gr () p -> Gr () Int
transformToRealGraph g = mkGraph (labNodes g) (map (\(s, t) -> (s, t, 1)) (edges g))

transformToPairs :: [Ref] -> [(Ref, Ref)]
transformToPairs (s:t:xs) = (s, t) : transformToPairs (t:xs)
transformToPairs _ = []

getPathFromRootToCurr :: Explorer p m c -> Gr () p
getPathFromRootToCurr e = mkGraph nl (filter ((\path -> \(s, t, _) -> (s, t) `elem` path) (transformToPairs n)) (labEdges $ execEnv e))
  where
    n = fromMaybe [] (sp 1 (currRef e) (transformToRealGraph (execEnv e)))
    nl = filter (\(i, _) -> i `elem` n) (labNodes (execEnv e))

--subExecEnvL :: Int -> Explorer p m c -> Gr () p
--subExecEnvL level e =

-- TODO: Handle shared nodes. a -> b -> a is not handled well now if we target (a, b).
mapOut :: Gr () p -> [Ref] -> Ref -> (Ref, Ref, p) -> Maybe [[(Ref, Ref, p)]]
mapOut gr visited goal (s, t, l)
  | goal == t = Just [[(s, t, l)]]
  | otherwise = case t `elem` visited of
          True  -> Nothing
          False -> Just $ map ((:)(s, t, l)) (concat $ catMaybes $ map (mapOut gr (t : visited) goal) (out gr t))
                                  


getPathsFromTo :: Ref -> Ref -> Explorer p m c -> [[(Ref, Ref, p)]]
getPathsFromTo from to exp = concat $ catMaybes $ map (mapOut (execEnv exp) [from] to) (out (execEnv exp) from)
