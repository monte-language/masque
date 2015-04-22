{-# LANGUAGE DeriveFunctor #-}

module Masque.Objects where

import Control.Object
import Data.Functor.Sum

data InternalMessage = GetStorage
    deriving (Enum, Eq, Ord, Show)

data Storage = NoStorage
             | DoubleStorage Double
             | IntStorage Integer
    deriving (Eq, Ord, Show)

-- Messages are parametized over the environment of objects and a functor
-- fulfillment type.
data Message m a = Message { _verb :: String
                           , _arguments :: [MObject m]
                           , _payload :: a }
    deriving (Functor)

-- Objects can be moved between different environments, so the environment of
-- a Monte object is a parameter that can change.
type MObject m = Object (Sum (Message m) (Request InternalMessage Storage)) m
