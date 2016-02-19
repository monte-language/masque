{-# LANGUAGE TemplateHaskell #-}

module Masque.Monte where

import Control.Lens
import Control.Monad.Trans.Either
import Control.Monad.Trans.RWS
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as S
import Data.Unique

import Masque.Objects
import Masque.Vats

data Err = Refused
         | Unsettled
         | BadWrite String
         | Ejecting Unique Obj
         | BadName String (S.Set String)

instance Show Err where
    show Refused = "Refused"
    show Unsettled = "Unsettled"
    show (BadWrite name) = "BadWrite " ++ show name
    show (Ejecting _ _) = "Ejecting"
    show (BadName name _) = "BadName " ++ show name


data MonteState = MS { _envStack :: NonEmpty Env
                     , _vat :: Vat }

makeLenses ''MonteState


-- | The monad of actions taken by Monte objects in an environment, resulting
--   in either objects or errors.
type Monte = EitherT Err (RWST Env () MonteState IO)
