{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Masque.Objects.Ejectors where

import Control.Monad
import Control.Object
import Data.Functor.Sum
import Data.Unique
import Masque.Objects

ejector :: Unique -> MObject EjEnv
ejector u = Object $ \case
    InL (MMessage verb args x) -> fmap (x,) $ do
        case (verb, args) of
            ("run", [value]) -> EjEnv (Left (Ejecting u value))
            _ -> mzero
    InR req -> mirandaLL req $ ejector u
