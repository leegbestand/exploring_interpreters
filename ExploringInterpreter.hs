module ExploringInterpreter 
    ( Explorer
    , execute
    , executeAll
    , revert
    , displayDot
    , display
    , displayExecEnv
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
    ) where

import Data.Graph.Inductive.Graph 
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query

import qualified Data.IntMap as IntMap
import Data.List

type Ref = Int

data Explorer programs configs = Explorer { 
    -- Currently part of the Data and not the type. See if it possible to make part of the type.
    sharing :: Bool,
    backTracking :: Bool,
    defInterp :: programs -> configs -> configs, 
    -- Possible definition for interpM, needs M to be generic.
    -- defInterpM :: programs -> configs -> IO configs, 
    config :: configs, -- Cache the config
    currRef :: Ref,
    genRef :: Ref,
    cmap :: IntMap.IntMap configs,
    execEnv :: Gr () programs
}

-- Constructor for a exploring interpreter.
mkExplorer :: Bool -> Bool -> (a -> b -> b) -> b -> Explorer a b
mkExplorer share backtrack definterp conf = Explorer 
    { defInterp = definterp
    , config = conf
    , genRef = 1 -- Currently generate reference by increasing a counter.
    , currRef = 1
    , cmap = IntMap.fromList [(1, conf)]
    , execEnv = mkGraph [(1, ())] []
    , sharing = share 
    , backTracking = backtrack
}

mkExplorerStack :: (a -> b -> b) -> b -> Explorer a b 
mkExplorerStack = mkExplorer False True

mkExplorerTree  :: (a -> b -> b) -> b -> Explorer a b 
mkExplorerTree  = mkExplorer False False

mkExplorerGraph :: (a -> b -> b) -> b -> Explorer a b 
mkExplorerGraph = mkExplorer True False 

deref :: Explorer p c -> Ref -> Maybe c
deref e r = IntMap.lookup r (cmap e)

findRef :: Eq c => Explorer p c -> c -> Maybe (Ref, c)
findRef e c = find (\(r, c') -> c' == c) (IntMap.toList (cmap e))

addNewPath :: Explorer p c -> p -> c -> Explorer p c
addNewPath e p c = e { config = c, currRef = newref, genRef = newref, cmap = IntMap.insert newref c (cmap e),
     execEnv = insNode (newref, ()) $ insEdge (currRef e, newref, p) (execEnv e)}
     where newref = genRef e + 1

-- Check if we already have an edge from the current node, if not create a new one.
handleRef :: Eq p => Explorer p c -> p -> (Ref, c) -> Explorer p c
handleRef e p (r, c) = if (currRef e, r, p) `elem` out (execEnv e) (currRef e)
    then e { currRef = r, config = c }
    else addNewPath e p c

updateConf :: Eq c => Eq p => Explorer p c -> (p, c) -> Explorer p c
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

execute :: Eq c => Eq p =>  p -> Explorer p c -> Explorer p c
execute p e = updateConf e (p, newconf) 
    where newconf = defInterp e p (config e)

executeAll :: (Eq c, Eq p) => [p] -> Explorer p c -> Explorer p c
executeAll ps e = foldr execute e ps

-- Implementation for execute with Monad, can replace execute if
-- monads are part of the explorer type.
{-- executeM :: (Eq c, Eq p) => p -> Explorer p c -> IO (Explorer p c)
executeM p e  = do
    c' <- defInterpM e p (config e)
    return $ updateConf e (p, c')
--}

    
deleteMap :: [Ref] -> IntMap.IntMap a -> IntMap.IntMap a
deleteMap xs m = foldl (flip IntMap.delete) m xs

revert :: Ref -> Explorer p c -> Maybe (Explorer p c)
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
    

displayDot :: Show p => Explorer p c  -> IO ()
displayDot g = do
    putStrLn "digraph g {"
    mapM_ (displayDotVertices (currRef g)) (nodes (execEnv g))
    mapM_ displayDotEdge (labEdges (execEnv g))
    putStrLn "}"

-- TODO: define in terms of displayGr
display :: Show p => Explorer p c -> String
display e = "{\n\"edges\": \"" ++ show (labEdges (execEnv e)) ++ "\",\n"
          ++ "\"vertices\": \"" ++ show (nodes (execEnv e)) ++ "\",\n"
          ++ "\"current\": \"" ++ (show (currRef e)) ++ "\"\n"
          ++ "}"

displayExecEnv :: Show p => Gr () p -> String
displayExecEnv gr = "{\n\"edges\": \"" ++ show (labEdges gr) ++ "\",\n"
                  ++ "\"vertices\": \"" ++ show (nodes gr) ++ "\",\n"
                  ++ "}"

subExecEnv :: Explorer p c -> Gr () p
subExecEnv e = subgraph (foldr (\(s, t) l ->  s : t : l) [] (filter (\(_, t) -> t == (currRef e)) (edges (execEnv e)))) (execEnv e)
