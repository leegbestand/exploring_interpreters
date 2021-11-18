module Language.Explorer.Tools.REPL where 

import Control.Monad.IO.Class (MonadIO(..))
import Language.Explorer.Monadic
    (Explorer(config), execute, revert, jump, toTree)
import qualified System.Console.Readline as Rl
import Data.Tree (drawTree)
import Data.Maybe (fromJust, isNothing)
import Data.Char (isSpace)
import Data.List (find, isPrefixOf) 
import Text.Read (readMaybe)
import Control.Arrow (Arrow(first))



type MetaTable p m c o = [(String, String -> Explorer p m c o -> m (Explorer p m c o))]
type RParser p c = String -> c -> Maybe p
type Prompt p m c o = Explorer p m c o -> String
type MetaHandler p m c o = String -> Explorer p m c o -> m (Explorer p m c o)
type OutputHandler m o = o -> m ()
type Repl p m c o = Prompt p m c o -> RParser p c -> String -> MetaTable p m c o -> MetaHandler p m c o -> OutputHandler m o -> Explorer p m c o -> m ()


handleJump :: MonadIO m => String -> Explorer p m c o -> m (Explorer p m c o)
handleJump input ex = case readMaybe (dropWhile isSpace input) of
  (Just ref_id) -> case jump ref_id ex of
    (Just ex') -> return ex'
    Nothing -> liftIO $ putStrLn "Given reference is not in the exploration tree." >> return ex
  Nothing -> liftIO $ putStrLn "the jump command requires an integer argument." >> return ex

handleRevert :: MonadIO m => MetaHandler p m c o
handleRevert input ex = case readMaybe (dropWhile isSpace input) of
  (Just ref_id) -> case revert ref_id ex of
    (Just ex') -> do
      liftIO $ putStrLn "reverting"
      return ex'
    Nothing -> liftIO $ putStrLn "Given reference is not valid for reverting." >> return ex
  Nothing -> liftIO $ putStrLn "the jump command requires an integer argument." >> return ex

handleTree :: MonadIO m => MetaHandler p m c o
handleTree input ex = do
  liftIO $ putStrLn . drawTree $ fmap (show . fst) (toTree ex)
  return ex

metaTable :: MonadIO m => [(String, String -> Explorer p m c o -> m (Explorer p m c o))]
metaTable = [
  ("jump", handleJump),
  ("revert", handleRevert),
  ("tree", handleTree)]

constructMetaTable :: MonadIO m => String -> [(String, String -> Explorer p m c o -> m (Explorer p m c o))]
constructMetaTable prefix = map (first (prefix ++ )) metaTable

repl :: (Eq p, Eq o, Monoid o, MonadIO m) => Repl p m c o
repl prompt parser metaPrefix metaTable metaHandler outputHandler ex = do
  minput <- liftIO . Rl.readline . prompt $ ex
  case minput of
    (Just input) -> do
      liftIO $ Rl.addHistory input
      if metaPrefix `isPrefixOf` input then runMeta input else runExec input
    Nothing -> return ()
  where
    repl' = repl prompt parser metaPrefix metaTable metaHandler outputHandler
    runMeta input =
      let (pcmd, args) = break isSpace input in
        case find (\(cmd, _) -> (metaPrefix ++ cmd) == pcmd) metaTable of
          Just (_, f) -> f args ex >>= repl'
          Nothing -> metaHandler input ex >>= repl'
    runExec input =
      case parser input (config ex) of
        (Just program) -> execute program ex >>= \(newEx, out) -> (outputHandler out >> repl' newEx)
        Nothing -> repl' ex
