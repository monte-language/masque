module Masque.Objects.Builtin where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.IORef
import Data.Maybe

import Masque.Monte

pureObj :: (String -> [Obj] -> [(Obj, Obj)] -> Maybe (Monte Obj)) ->
           Obj
pureObj maction = Obj $ \verbs args namedArgs ->
    fromMaybe refuse (maction verbs args namedArgs)

constObj :: e ->
            (String -> [Obj] -> [(Obj, Obj)] -> Maybe (ReaderT e Monte Obj)) ->
            Obj
constObj env maction = Obj $ \verbs args namedArgs ->
    case maction verbs args namedArgs of
        Just action -> runReaderT action env
        Nothing     -> refuse

stateObj :: s ->
            (String -> [Obj] -> [(Obj, Obj)] -> Maybe (StateT s Monte Obj)) ->
            IO Obj
stateObj initialState maction = do
    ref <- newIORef initialState
    return . Obj $ \verbs args namedArgs ->
        case maction verbs args namedArgs of
            Just action -> do
                s <- liftIO $ readIORef ref
                (obj, s') <- runStateT action s
                liftIO $ writeIORef ref s'
                return obj
            Nothing     -> refuse
