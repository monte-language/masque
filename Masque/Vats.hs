{-# LANGUAGE TemplateHaskell #-}

module Masque.Vats where

import Control.Lens

import Masque.Objects

-- XXX That empty () is for resolvers, once we have resolvers.
data Vat = Vat { _vatQueue :: [(Obj, String, [Obj], [(Obj, Obj)], ())]
               , _vatName :: String }
    deriving (Show)

makeLenses ''Vat
