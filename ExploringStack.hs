-- |

module ExploringStack where


-- TODO: move to own module.
-- Currently uses a list(stack) as the execution graph.
data Explorer programs configs = Explorer { defInterp :: programs -> configs -> configs, config :: configs, execEnv:: [(configs, programs)]}

-- Constructor for a exploring interpreter.
build :: (a -> b -> b) -> b -> Explorer a b
build definterp conf = Explorer {defInterp = definterp, config = conf , execEnv = []}

execute :: Explorer p c -> p -> Explorer p c
execute int prog = int { config = newconfig, execEnv = (oldconfig, prog) : execEnv int}
    where oldconfig = config int
          newconfig = defInterp int prog oldconfig

displayEdge :: Show c => Show p => (c, p) ->  String
displayEdge (c1, p) = show c1 ++ "\n" ++ "|\n" ++ "|\n| " ++ show p ++ "\n|\n|\nv\n"


display :: Show c => Show p => Explorer p c -> IO ()
display explorer =  case currExecEnv of
    [] -> putStrLn $ show config explorer
    _  -> putStrLn $ concatMap displayEdge currExecEnv ++ show (config explorer)
    where currExecEnv = execEnv explorer


-- Destroys future steps(relative to the y in revert) when reverting, but keeps the history(before the y in revert) alive.
revert :: Eq c => Explorer p c -> c -> Explorer p c
revert explorer newconfig = if newconfig == config explorer then explorer else
    explorer { config = newconfig, execEnv = newgraph }
    where (_:newgraph) = dropWhile (\(c, _) -> c /= newconfig) (execEnv explorer)
