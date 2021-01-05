module Explorer where

import Data.Graph.Inductive.Graph 
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query

import qualified Data.IntMap as IntMap
import Data.List

type Ref = Int

data Equality = Reference | Structural

data Explorer programs configs = Explorer { 
    -- Currently part of the Data and not the type. See if it possible to make part of the type.
    equality :: Equality,
    backTracking :: Bool,
    defInterp :: programs -> configs -> configs, 
    config :: configs, -- Cache the config
    currRef :: Ref,
    genRef :: Ref,
    cmap :: IntMap.IntMap configs,
    execEnv :: Gr () programs
}

-- Constructor for a exploring interpreter.
build :: Equality -> Bool -> (a -> b -> b) -> b -> Explorer a b
build equal backtrack definterp conf = Explorer 
    { equality = equal
    , backTracking = backtrack
    , defInterp = definterp
    , config = conf
    , genRef = 1 -- Currently generate reference by increasing a counter.
    , currRef = 1
    , cmap = IntMap.fromList [(1, conf)]
    , execEnv = mkGraph [(1, ())] []
}

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

execute :: Eq c => Eq p => Explorer p c -> p -> Explorer p c
execute e p = case findRef e newconf of
    Just (r, c) -> case equality e of
        Structural -> if hasLEdge (execEnv e) (currRef e, r, p) 
                      then e { config = newconf, currRef = r }
                      else e { config = newconf, currRef = r, execEnv = insEdge (currRef e, r, p) (execEnv e) }
        Reference -> handleRef e p (r, newconf)
    Nothing -> addNewPath e p newconf
    where newconf = defInterp e p (config e)

    
deleteMap :: [Ref] -> IntMap.IntMap a -> IntMap.IntMap a
deleteMap xs m = foldl (flip IntMap.delete) m xs

revert'' :: Explorer p c -> (Ref, c) -> Explorer p c
revert'' e (r, c) = e { execEnv = execEnv', currRef = r, config = c, cmap = cmap'}
    where nodesToDel = reachable r (execEnv e) \\ [r]
          edgesToDel = filter (\(s, t) -> s `elem` nodesToDel || t `elem` nodesToDel) (edges (execEnv e))
          execEnv'   = (delEdges edgesToDel . delNodes nodesToDel) (execEnv e)
          cmap'      = deleteMap nodesToDel (cmap e)

revert' :: Explorer p c -> (Ref, c) -> Explorer p c
revert'  e (r, c) = if backTracking e then revert'' e (r, c) else e { currRef = r, config = c }

revert :: Explorer p c -> Ref -> Explorer p c
revert e r = case IntMap.lookup r (cmap e) of
    Just c -> revert' e (r, c)
    Nothing -> e -- For now do nothing on revert to non-existing Ref.

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

display :: Show p => Explorer p c -> IO ()
display e = prettyPrint (execEnv e)
