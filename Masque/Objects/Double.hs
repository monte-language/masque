module Masque.Objects.Double where

import Masque.Monte

ordering :: Ord a => a -> a -> Monte Obj
ordering x y = wrapInt $ case x `compare` y of
    LT -> -1
    EQ -> 0
    GT -> 1

nan :: Monte Obj
nan = wrapDouble $ 0 / 0

-- | Pass a message to a Double.
callDouble :: Double -> String -> [Obj] -> [(Obj, Obj)]-> Monte Obj
-- Unary.
callDouble d "abs" [] _ = wrapDouble $ abs d
callDouble d "negate" [] _ = wrapDouble $ negate d
callDouble d "sqrt" [] _ = wrapDouble $ sqrt d
-- Binary.
callDouble d "add" [obj] _ = do
    x <- coerceDouble obj
    wrapDouble $ d + x
callDouble d "approxDivide" [obj] _ = do
    x <- coerceDouble obj
    wrapDouble $ d / x
callDouble d "multiply" [obj] _ = do
    x <- coerceDouble obj
    wrapDouble $ d * x
callDouble d "subtract" [obj] _ = do
    x <- coerceDouble obj
    wrapDouble $ d - x
callDouble d "op__cmp" [obj] _ = do
    x <- coerceDouble obj
    if isNaN d || isNaN x then nan else ordering d x
callDouble _ _ _ _ = refuse
