{-# LANGUAGE TemplateHaskell #-}

module Masque.Monte where

import Control.Lens
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Unique

import Masque.Objects
import Masque.Vats

data Err = Refused
         | WrongType
         | Unsettled
         | Ejecting Unique Obj
         | BadWrite String
         | BadName String (S.Set String)

instance Show Err where
    show Refused = "Refused"
    show WrongType = "WrongType"
    show Unsettled = "Unsettled"
    show (BadWrite name) = "BadWrite " ++ show name
    show (Ejecting _ _) = "Ejecting"
    show (BadName name _) = "BadName " ++ show name


data MonteState = MS { _envStack :: NonEmpty Env
                     , _vat :: Vat }
    deriving (Show)

makeLenses ''MonteState

-- | The monad of actions taken by Monte objects in an environment, resulting
--   in either objects or errors.
type Monte = EitherT Err (StateT MonteState IO)

-- | Run a Monte computation with an environment, producing the components of
--   the computation in `IO`.
runMonte :: Monte a -> NonEmpty Env -> IO (Either Err a, MonteState)
runMonte action envs = runStateT (runEitherT action) (MS envs (Vat [] "someVat"))

-- | Run a Monte computation with an empty environment.
runPureMonte :: Monte a -> IO (Either Err a, MonteState)
runPureMonte action = runMonte action (Env M.empty :| [])

-- | Refuse to handle a message. Typical reasons of refusal include not having
--   a method which corresponds to the message, not having any applicable
--   matchers, etc.
--
--   For handling wrong types in input values, such as when unwrapping, use
--   `WrongType` or one of its wrappers.
refuse :: Monte ()
refuse = left Refused

unwrapBool :: Obj -> Monte Bool
unwrapBool (BoolObj b) = return b
unwrapBool _ = left WrongType

unwrapChar :: Obj -> Monte Char
unwrapChar (CharObj c) = return c
unwrapChar _ = left WrongType

unwrapDouble :: Obj -> Monte Double
unwrapDouble (DoubleObj d) = return d
unwrapDouble _ = left WrongType

unwrapInt :: Obj -> Monte Integer
unwrapInt (IntObj i) = return i
unwrapInt _ = left WrongType

unwrapStr :: Obj -> Monte String
unwrapStr (StrObj s) = return s
unwrapStr _ = left WrongType
