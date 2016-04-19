module Masque.Objects.Int where

import Data.Bits

import Masque.Monte

-- XXX I don't have the gumption to clean this up right now
callInt :: Integer -> String -> [Obj] -> [(Obj, Obj)] -> Monte Obj
-- Unary
-- Binary
-- callInt i "op__cmp" [DoubleObj d] _ = Just $ cmp (realToFrac i) d
-- callInt i "op__cmp" [IntObj j] _ = Just $ cmp i j
callInt i "add" [IntObj j] _ = wrapInt $ i + j
callInt i "and" [IntObj j] _ = wrapInt $ i .&. j
callInt i "approxDivide" [DoubleObj d] _ = wrapDouble $ realToFrac i / d
callInt i "floorDivide" [IntObj j] _ = wrapInt $ i `div` j
callInt i "multiply" [DoubleObj d] _ = wrapDouble $ d * realToFrac i
callInt i "multiply" [IntObj j] _ = wrapInt $ i * j
callInt i "pow" [IntObj j] _ = wrapInt $ i ^ j
callInt i "subtract" [IntObj j] _ = wrapInt $ i - j
-- Comparison
callInt i "aboveZero" [] _ = wrapBool $ i > 0
callInt i "atLeastZero" [] _ = wrapBool $ i >= 0
callInt i "atMostZero" [] _ = wrapBool $ i <= 0
callInt i "belowZero" [] _ = wrapBool $ i < 0
callInt i "isZero" [] _ = wrapBool $ i == 0
callInt _ _ _ _ = refuse
