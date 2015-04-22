{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Masque.Objects.Data where

import Control.Monad
import Control.Object
import Data.Char
import Data.Functor.Sum
import Data.List
import Masque.Objects

-- The monad for data objects permits no monadic effects other than tracking
-- failure. Since failure messages must not propagate, Maybe is used instead
-- of Either so that failure messages are dropped on the floor.
type DataEnv = Maybe

-- Get the internal storage of an object.
getStorage :: Monad m => MObject m -> m Storage
getStorage obj = do
    (storage, _) <- obj @- InR (request GetStorage)
    return storage

null :: MObject DataEnv
null = Object $ \case
    _ -> Nothing

bool :: Bool -> MObject DataEnv
bool b = Object $ \case
    InL (Message verb args x) -> fmap (x,) $ do
        case (verb, args) of
            ("not", []) -> return $ bool (not b)
            ("pick", [left, right]) -> return $ if b then left else right
            _           -> Nothing
    _ -> Nothing

char :: Char -> MObject DataEnv
char c = Object $ \case
    InL (Message verb args x) -> fmap (x,) $ do
        case (verb, args) of
            ("add", [other]) -> do
                (IntStorage i, _) <- other @- InR (request GetStorage)
                return $ char (chr (ord c + fromIntegral i))
            _ -> Nothing
    _ -> Nothing

asDouble :: MonadPlus m => MObject m -> m Double
asDouble obj = do
    storage <- getStorage obj
    case storage of
        DoubleStorage d -> return d
        IntStorage i    -> return $ fromIntegral i
        _               -> mzero

double :: Double -> MObject DataEnv
double d = Object $ \case
    InL (Message verb args x) -> fmap (x,) $ do
        case (verb, args) of
            ("abs", []) -> return $ double (abs d)
            ("add", [other]) -> do
                e <- asDouble other
                return $ double (d + e)
            ("multiply", [other]) -> do
                e <- asDouble other
                return $ double (d * e)
            ("negate", []) -> return $ double (negate d)
            ("sqrt", []) -> return $ double (sqrt d)
            ("subtract", [other]) -> do
                e <- asDouble other
                return $ double (d - e)
            _ -> Nothing
    InR req -> do
        r <- accept (\GetStorage -> return $ DoubleStorage d) req
        return (r, double d)
-- callDouble d "op__cmp" [preview _DoubleObj -> Just d'] = Just $ cmp d d'

asInt :: MonadPlus m => MObject m -> m Integer
asInt obj = do
    storage <- getStorage obj
    case storage of
        IntStorage i -> return i
        _            -> mzero

int :: Integer -> MObject DataEnv
int i = Object $ \case
    InL (Message verb args x) -> fmap (x,) $ do
        case (verb, args) of
            ("add", [other]) -> do
                (IntStorage j, _) <- other @- InR (request GetStorage)
                return $ int (i + j)
            _ -> Nothing
    InR req -> do
        r <- accept (\GetStorage -> return $ IntStorage i) req
        return (r, int i)

str :: String -> MObject DataEnv
str s = Object $ \case
    InL (Message verb args x) -> fmap (x,) $ do
        case (verb, args) of
            ("get", [index]) -> do
                i <- asInt index
                guard (0 <= i && i < genericLength s)
                return $ char (genericIndex s i)
            ("size", []) -> return $ int (genericLength s)
            _ -> Nothing
    _ -> Nothing
-- callStr s "add" [CharObj c] = Just . StrObj $ s ++ [c]
-- callStr s "add" [StrObj t] = Just . StrObj $ s ++ t
-- callStr s "multiply" [IntObj i] =
--     Just . StrObj . concat $ replicate (fromIntegral i) s
