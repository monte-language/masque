module Masque.Objects.Bool where

import Masque.Monte

-- | Pass a message to a Bool.
callBool :: Bool -> String -> [Obj] -> [(Obj, Obj)] -> Monte Obj
-- Unary.
callBool b "not" [] _ = wrapBool $ not b
-- Binary.
callBool b "and" [other] _ = do
    x <- unwrapBool other
    wrapBool $ b && x
callBool b "butNot" [other] _ = do
    x <- unwrapBool other
    wrapBool $ b && not x
callBool b "or" [other] _ = do
    x <- unwrapBool other
    wrapBool $ b || x
callBool b "xor" [other] _ = do
    x <- unwrapBool other
    wrapBool $ b /= x
-- And other methods.
callBool b "pick" [l, r] _ = return $ if b then l else r
callBool _ _ _ _ = refuse
