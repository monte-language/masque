module Masque.Objects.Ejector where

import Masque.Monte

makeEj :: Unique -> IO Obj
makeEj u = stateObj True $ \verb args namedArgs -> do
    guard $ null namedArgs
    case (verb, args) of
        ("run", [obj])  -> left $ Ejecting u obj
        ("run", [])     -> left $ Ejecting u NullObj
        ("disable", []) -> do
            put False
            return NullObj
