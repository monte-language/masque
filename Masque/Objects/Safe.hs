module Masque.Objects.Safe where

import Control.Monad
import Control.Monad.Trans.Either

import Masque.Ejectors
import Masque.Monte
import Masque.Objects.Builtin

theThrow :: Obj
theThrow = pureObj $ \verb args namedArgs -> do
    guard $ null namedArgs
    case (verb, args) of
        ("run",   [prob])     -> Just . left $ Exception prob
        ("eject", [ej, prob]) -> Just $ do
            throwEject ej prob
            left $ Exception prob
        _                     -> Nothing
