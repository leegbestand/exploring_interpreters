{-# LANGUAGE GADTs, ConstraintKinds, KindSignatures #-}
module Language.Explorer.GStorage where 


{-
Gives a possible implementation of the explorer in terms of a 
generic storage device as defined by Data.Storage
-}

import Data.Storage 
import Control.Monad.State


type Language p m c o = (Eq p, Eq o, Monad m, Monoid o)


data Explorer storage programs m configs output where 
    Explorer :: (Language programs m configs output, Storage m storage) =>
        { store :: storage configs programs output
        , defInterp :: programs -> configs -> m (Maybe configs, output)
        , config :: configs 
        , currRef :: Ref 
        } -> Explorer storage programs m configs output


mkExplorer :: (Language p m c o, Storage m s) =>
    (p -> c -> m (Maybe c, o)) -> c -> (s c p o) -> m (Explorer s p m c o)
mkExplorer defInterp conf state = do 
    (ref, s') <- runStateT (new conf) state
    let ex = Explorer { defInterp = defInterp 
                 , store = s'
                 , config = conf
                 , currRef = ref
                 }
    return ex


execute :: (Language p m c o, Storage m s) =>  p -> Explorer s p m c o -> m (Explorer s p m c o, o)
execute p e =
  do (mcfg, o) <- defInterp e p (config e)
     case mcfg of
       Just cfg -> do
            s' <- execStateT (new cfg >>= \r' -> link (currRef e) (p, o) r') (store e)
            return (e { store = s'}, o)
       Nothing  -> return (e, o)


jump :: (Monad m, Storage m s) => Ref -> Explorer s p m c o -> m (Maybe (Explorer s p m c o))
jump r e = do 
    (mr', s') <- runStateT (query r) (store e) 
    case mr' of 
        Just c -> return . Just $ e { config = c, currRef = r, store = s'}
        Nothing -> return Nothing -- This might be incorrect due to effects inside the monad? Maybe change from Maybe to (Explorer, Bool)
    


revert :: 