-- |

module ExploringStack where


-- TODO: move to own module.
-- Currently uses a list(stack) as the execution graph.
data Explorer programs configs = Explorer { defInterp :: programs -> configs -> configs, config :: configs, execEnv:: [(configs, programs)]}

-- Constructor for a exploring interpreter.
build :: (a -> b -> b) -> b -> Explorer a b
build definterp conf = Explorer {defInterp = definterp, config = conf , execEnv = []}

explore :: Explorer p c -> p -> Explorer p c
explore int prog = int { config = newconfig, execEnv = execEnv int ++ [(oldconfig, prog)]}
    where oldconfig = config int
          newconfig = defInterp int prog oldconfig

displayEdge :: Show c => Show p => (c, p) ->  String
displayEdge (c1, p) = show c1 ++ "\n" ++ "|\n" ++ "|\n| " ++ show p ++ "\n|\n|\nv\n"

display :: Show c => Show p => Explorer p c -> IO ()
display explorer = case execEnv explorer of
    [] -> print (config explorer)
    _ -> putStrLn $ concatMap displayEdge (execEnv explorer) ++ show (config explorer)


-- Destroys future steps(relative to the y in revert) when reverting, but keeps the history(before the y in revert) alive.
revert :: Eq c => Explorer p c -> c -> Explorer p c
revert explorer newconfig = explorer { config = newconfig, execEnv = newgraph }
    where newgraph = if length prunedGraph == length (execEnv explorer) then [] else prunedGraph
          prunedGraph = takeWhile (\(c, _) -> c /= newconfig) (execEnv explorer)
