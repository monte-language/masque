module Masque.Objects.Bool where

import Masque.Monte
import Masque.Objects

-- | Wrap a Bool in a Monte object.
returnBool :: Bool -> Monte Obj
returnBool = return . BoolObj

-- | Pass a message to a Bool.
callBool :: Bool -> String -> [Obj] -> [(Obj, Obj)] -> Monte Obj
-- Unary.
callBool b "not" [] _ = returnBool $ not b
-- Binary.
callBool b "and" [other] _ = do
    x <- unwrapBool other
    returnBool $ b && x
callBool b "butNot" [other] _ = do
    x <- unwrapBool other
    returnBool $ b && not x
callBool b "or" [other] _ = do
    x <- unwrapBool other
    returnBool $ b || x
callBool b "xor" [other] _ = do
    x <- unwrapBool other
    returnBool $ b /= x
-- And other methods.
callBool b "pick" [l, r] _ = return $ if b then l else r
callBool _ _ _ _ = refuse
