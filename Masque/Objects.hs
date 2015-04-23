{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Masque.Objects where

import Control.Monad
import Control.Object
import Data.Functor.Sum
import Data.Unique

data Storage = NoStorage
             | DoubleStorage Double
             | IntStorage Integer
    deriving (Eq, Show)

-- Messages are parametized over the environment of objects and a functor
-- fulfillment type.
data MMessage m a = MMessage { _verb :: String, _arguments :: [MObject m], derp :: a }
    deriving (Functor)

data LLMessage = GetStorage

data LLResponse = RStorage Storage
    deriving (Eq, Show)

-- Default implementation of low-level messages.
mirandaLL :: (Functor m, Monad m)
          => Request LLMessage LLResponse r -> MObject m -> m (r, MObject m)
mirandaLL req self = do
    r <- flip accept req $ \case
        GetStorage -> return $ RStorage NoStorage
    return (r, self)

-- Objects can be moved between different environments, so the environment of
-- a Monte object is a parameter that can change.
-- The two different levels of messages here are due to the separation between
-- Monte-level messages and messages that are necessarily lower-level. For
-- example, requesting the internal storage/closure of an object is not
-- possible to do on the Monte level.
type MObject m = Object (Sum (MMessage m) (Request LLMessage LLResponse)) m

-- The monad for data objects permits no monadic effects other than tracking
-- failure. Since failure messages must not propagate, Maybe is used instead
-- of Either so that failure messages are dropped on the floor.
type DataEnv = Maybe

-- The type of errors which can occur.
data Err m = NoReason
           | Ejecting Unique (MObject m)
--         | Unsettled
--         | BadName String (S.Set String)
--         | BadWrite String
--         | Refused (MObject m) String [MObject m] (S.Set String)

-- Natural transformation on the front half of a Sum.
transSumFirst :: (forall x. f x -> g x) -> Sum f h a -> Sum g h a
transSumFirst eta (InL x) = InL $ eta x
transSumFirst _ (InR x) = InR x

-- A single extension is needed to carry some data for ejectors.
newtype EjEnv a = EjEnv { unEjEnv :: Either (Err EjEnv) a }
    deriving (Functor)

instance Monad EjEnv where
    return = EjEnv . Right
    EjEnv (Left err) >>= _ = EjEnv (Left err)
    EjEnv (Right a) >>= f = f a

instance MonadPlus EjEnv where
    mzero = EjEnv (Left NoReason)
    mplus (EjEnv (Left _)) x = x
    mplus x _ = x

dataToEj :: MObject DataEnv -> MObject EjEnv
dataToEj obj = transSumFirst messages ^>>@ obj @>>^ environment
    where
    messages (MMessage verb args x)
        = MMessage verb (map ejToData args) x
    environment = maybe (EjEnv (Left NoReason)) (EjEnv . Right)

ejToData :: MObject EjEnv -> MObject DataEnv
ejToData obj = transSumFirst messages ^>>@ obj @>>^ environment
    where
    messages (MMessage verb args x)
        = MMessage verb (map dataToEj args) x
    environment = either (const Nothing) Just . unEjEnv
