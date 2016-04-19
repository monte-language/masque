module Masque.Ejectors where

import Control.Monad.Error.Class
import Control.Monad.Trans.Either
import Data.Unique

import Masque.Monte

-- | Run an action, catching a single specified ejector. If caught, the
--   ejector's payload will be run through the given handler action.
catchEjector :: Unique -> Monte Obj -> (Obj -> Monte Obj) -> Monte Obj
catchEjector u action handler = catchError action $ \err ->
    case err of
        Ejecting u' obj | u == u' -> handler obj
        _                         -> left err

-- | Like Monte m`throw.eject(ej, problem)`.
throwEject :: Obj -> Obj -> Monte ()
throwEject ej problem = left $ case ej of
    EjectorObj u -> Ejecting u problem
    _            -> Exception problem
