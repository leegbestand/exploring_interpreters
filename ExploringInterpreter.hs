
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

import qualified ExploringInterpreterM as ExplorerM
import Control.Monad.Identity

import qualified Data.IntMap as IntMap
import Data.List
import Data.Foldable

-- We shadow instead of exporting directly to make the user interaction
-- the same.
type Ref = ExplorerM.Ref
type Gr = ExplorerM.Gr
type Explorer a b = ExplorerM.Explorer a Identity b

currRef :: Explorer a b -> Ref
currRef = ExplorerM.currRef

execEnv :: Explorer a b -> Gr () a
execEnv = ExplorerM.execEnv

config :: Explorer a b -> b
config = ExplorerM.config

deref :: Explorer p c -> Ref -> Maybe c
deref = ExplorerM.deref

-- This should be able with func composition.
wrap :: Monad m => (a -> b -> b) -> a -> b -> m b
wrap def p e = return $ def p e

-- TODO: Put types into variables(if possible?).
-- Constructor for a exploring interpreter.
mkExplorerStack:: (Show a, Eq a, Eq b) => (a -> b -> b) -> b -> Explorer a b
mkExplorerStack definterp conf = ExplorerM.mkExplorerStack (wrap definterp) conf

mkExplorerTree:: (Show a, Eq a, Eq b) => (a -> b -> b) -> b -> Explorer a b
mkExplorerTree definterp conf = ExplorerM.mkExplorerStack (wrap definterp) conf

mkExplorerGraph :: (Show a, Eq a, Eq b) => (a -> b -> b) -> b -> Explorer a b
mkExplorerGraph definterp conf = ExplorerM.mkExplorerStack (wrap definterp) conf

execute :: (Eq c, Eq p) =>  p -> Explorer p c -> Explorer p c
execute p e = runIdentity $ ExplorerM.execute p e

executeAll :: (Eq c, Eq p) => [p] -> Explorer p c -> Explorer p c
executeAll p e = runIdentity $ ExplorerM.executeAll p e

revert :: ExplorerM.Ref -> Explorer p c -> Maybe (Explorer p c)
revert = ExplorerM.revert

display :: Show p => Explorer p c -> String
display = ExplorerM.display

displayDot :: Show p => Explorer p c  -> IO ()
displayDot = ExplorerM.displayDot

displayExecEnv :: Show p => Gr () p -> String
displayExecEnv = ExplorerM.displayExecEnv

subExecEnv :: Explorer p c -> Gr () p
subExecEnv = ExplorerM.subExecEnv
