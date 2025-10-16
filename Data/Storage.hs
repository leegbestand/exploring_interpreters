{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ExplicitForAll #-}

module Data.Storage where 

import Control.Monad.State
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query
import Data.Graph.Inductive.Query.SP
import qualified Data.IntMap as IntMap

type Ref = Int


class Storage m s where 
    new :: forall c p o. c -> StateT (s c p o) m Ref
    query :: forall c p o. Ref -> StateT (s c p o) m (Maybe c)
    link :: forall c p o. Ref -> (p, o) -> Ref -> StateT (s c p o) m ()
    unlink :: forall c p o. Ref -> Ref -> StateT (s c p o) m ()
    -- All paths from r1 -> ?r2. If r2 is not specified, return all outgoing paths from r1.
    paths :: forall c p o. Ref -> Maybe Ref -> StateT (s c p o) m [(Ref, (p, o), Ref)] 



data GraphConfig configs programs output = 
    GraphConfig 
    { genRef :: Ref 
    , cmap :: IntMap.IntMap configs
    , execEnv :: Gr Ref (programs, output)
    }


instance Monad m => Storage m GraphConfig where 
    new c = do 
        s <- get 
        let newref = genRef s + 1
        put $ s { genRef = newref
                , execEnv = insNode (newref, newref) (execEnv s)
                , cmap = IntMap.insert newref c (cmap s)
                }
        return newref
    query ref = get >>= return . IntMap.lookup ref . cmap
    link source (p, o) target = 
        get >>= \s -> put $ s { execEnv = insEdge (source, target, (p, o)) (execEnv s)}
    unlink _ _ = return () 
    paths _ _ = return []
