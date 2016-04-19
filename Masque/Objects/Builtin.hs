module Masque.Objects.Builtin where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.IORef

import Masque.Monte

constObj :: e -> (String -> [Obj] -> [(Obj, Obj)] -> ReaderT e Monte Obj) -> Obj
constObj env action = Obj $ \verbs args namedArgs ->
    runReaderT (action verbs args namedArgs) env

stateObj :: s -> (String -> [Obj] -> [(Obj, Obj)] -> StateT s Monte Obj) -> IO Obj
stateObj initialState action = do
    ref <- newIORef initialState
    return . Obj $ \verbs args namedArgs -> do
        s <- liftIO $ readIORef ref
        (obj, s') <- runStateT (action verbs args namedArgs) s
        liftIO $ writeIORef ref s'
        return obj
