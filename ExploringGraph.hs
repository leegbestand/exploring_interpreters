-- |

module ExploringGraph where

import Data.List


data Graph programs configs = Graph {edges :: [(configs, programs, configs)], nodes :: [configs]} deriving (Show)

buildGraph :: [(configs, programs, configs)] -> [configs] -> Graph programs configs
buildGraph edges nodes = Graph {edges = edges, nodes = nodes}

graphAddVertices :: Eq v => Graph e v -> [v] -> Graph e v
graphAddVertices g [] = g
graphAddVertices g (v:vs) = 
    if v `elem` nodes g then graphAddVertices (g {nodes = nodes g ++ [v]}) vs else graphAddVertices g vs

graphAddEdge :: Eq e => Eq n => Graph e n -> (n, e, n) -> Graph e n
graphAddEdge g (s, e, t) = 
    if (s, e, t) `elem` edges g then g else graphAddVertices (g {edges = edges g ++ [(s, e, t)]}) [s, t]

displayGraphEdgeDot :: Show v => Show e => (v, e, v) -> IO ()
displayGraphEdgeDot (s, e, t) = do
    print (show s)
    putStr " -> "
    print (show t)
    putStr "[label="
    print (show e)
    putStrLn "]"

displayGraphDot :: Show e => Show v => Graph e v -> IO ()
displayGraphDot g = do
    putStrLn "digraph g {"
    mapM_ displayGraphEdgeDot (edges g)
    putStrLn "}"


data Explorer programs configs = Explorer { defInterp :: programs -> configs -> configs, config :: configs, execEnv :: Graph programs configs}

-- Constructor for a exploring interpreter.
build :: (a -> b -> b) -> b -> Explorer a b
build definterp conf = Explorer {defInterp = definterp, config = conf , execEnv = buildGraph [] [conf]}


execute :: Eq p => Eq c => Explorer p c -> p -> Explorer p c
execute int prog = case find (\(c, p, _) -> c == oldconfig && p == prog) (edges $ execEnv int) of
    Just (curr, _, new) -> int {config = new}
    Nothing -> int { config = newconfig, execEnv = graphAddEdge (execEnv int) (oldconfig, prog, newconfig)}
    where oldconfig = config int
          newconfig = defInterp int prog oldconfig

display :: Show c => Show p => Explorer p c -> IO ()
display explorer = displayGraphDot (execEnv explorer)

revert :: Eq c => Eq p => Explorer p c -> c -> Explorer p c
revert explorer newconfig = explorer { config = newconfig, execEnv = graphAddVertices (execEnv explorer) [newconfig]}
