module Masque.Equality where

import Control.Monad
import Data.Foldable (toList)

import Masque.Objects

-- | Whether two objects are extensionally equal. `Nothing` means that the
--   objects are incomparable due to being unsettled.
sameEver :: Obj -> Obj -> Maybe Bool
sameEver left right = case (left, right) of
    (NullObj, NullObj) -> return True
    (BoolObj x, BoolObj y) -> return $ x == y
    (CharObj x, CharObj y) -> return $ x == y
    (DoubleObj x, DoubleObj y) -> return $ x == y
    (IntObj x, IntObj y) -> return $ x == y
    (StrObj x, StrObj y) -> return $ x == y
    (EjectorObj x, EjectorObj y) -> return $ x == y
    -- Definitely incorrect in the case of recursive structures.
    (ConstListObj xs, ConstListObj ys) -> do
        bools <- zipWithM sameEver (toList xs) (toList ys)
        return $ and bools
    -- Definitely incorrect in the case of recursive structures.
    (ConstMapObj xs, ConstMapObj ys) -> do
        bools <- zipWithM zippingTuples xs ys
        return $ and bools
    -- Should consider auditors, not just object identity.
    (UserObj x _ _ _ _ _, UserObj y _ _ _ _ _) -> return $ x == y
    (_, _) -> return False
    where
    zippingTuples (k, v) (k', v') = do
        this <- sameEver k k'
        that <- sameEver v v'
        return $ this && that
