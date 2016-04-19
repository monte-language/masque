module Masque.Objects.Str where

import Control.Monad
import Data.List

import Masque.Monte

-- | Pass a message to a Str.
callStr :: String -> String -> [Obj] -> [(Obj, Obj)] -> Monte Obj
callStr s "add" [obj] _ = case obj of
    CharObj c -> wrapStr $ s ++ [c]
    StrObj x  -> wrapStr $ s ++ x
    _         -> refuse
callStr s "get" [obj] _ = do
    index <- unwrapInt obj
    when (index >= genericLength s) refuse
    wrapChar $ s `genericIndex` index
callStr s "multiply" [obj] _ = do
    count <- unwrapInt obj
    wrapStr . concat $ genericReplicate count s
callStr s "size" [] _ = wrapInt $ genericLength s
callStr _ _ _ _ = refuse
