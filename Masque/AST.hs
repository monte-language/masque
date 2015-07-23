{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Masque.AST where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Bits
import qualified Data.ByteString.Char8 as BSC
import Data.Data
import Data.Data.Lens
import Data.List
import Data.Word
import Text.PrettyPrint.GenericPretty

data Node = Null
          | CharNode Char
          | DoubleNode Double
          | IntNode Integer
          | StrNode String
          | Tuple [Node]
          | Assign String Node
          | BindingNode String
          | Call Node Node Node
          | Def Pattern Node Node
          | Escape Pattern Node Pattern Node
          | Finally Node Node
          | Hide Node
          | If Node Node Node
          | Matcher Pattern Node
          | Method Node Node [Pattern] Node Node
          | Noun String
          | Object Node Pattern Node Node
          | Script Node [Node] [Node]
          | Sequence [Node]
          | Try Node Pattern Node
    deriving (Data, Eq, Generic, Show, Typeable)

instance Out Node
instance Plated Node where
    plate = uniplate

data Pattern = Ignore Node
             | BindPattern String
             | Final String Node
             | ListPattern [Pattern]
             | Var String Node
             | Via Node Pattern
    deriving (Data, Eq, Generic, Show, Typeable)

instance Out Pattern
instance Plated Pattern where
    plate = uniplate

-- | Deserialization

unshift :: Word8 -> Word8
unshift = subtract 32

getByte :: Get Word8
getByte = liftM unshift getWord8

getVarInt :: Get Integer
getVarInt = do
    b <- getByte
    let rv = toInteger $ b .&. 0x7f
    if b .&. 0x80 == 0x00
        then return rv
        else do
            vi <- getVarInt
            return $ rv .|. vi `shiftL` 7

getDouble :: Get Double
getDouble = do
    bs <- replicateM 8 getByte
    let w64 = foldl' (\w x -> w `shiftL` 8 .|. fromIntegral x) 0 bs
    return $ wordToDouble w64

zzd :: Integer -> Integer
zzd i = if i .&. 1 == 1 then (i `div` 2) `xor` (-1) else i `div` 2

getPatternList :: Get [Pattern]
getPatternList = do
    7 <- getByte
    arity <- getVarInt
    replicateM (fromIntegral arity) getPattern

getPattern :: Get Pattern
getPattern = do
    tag <- getByte
    case tag of
        -- null pattern
        0  -> return $ Ignore Null
        27 -> Final <$> getNoun <*> getNode
        28 -> Ignore <$> getNode
        29 -> Var <$> getNoun <*> getNode
        30 -> do
            -- Must be a tuple
            ps <- getPatternList
            -- Discard the tail
            _ <- getPattern
            return $ ListPattern ps
        31 -> Via <$> getNode <*> getPattern
        32 -> BindPattern <$> getNoun
        _  -> fail "Not a pattern"

getNoun :: Get String
getNoun = do
    node <- getNode
    case node of
        StrNode s -> return s
        Noun s    -> return s
        _         -> fail "Not a string or noun"

getNode :: Get Node
getNode = do
    tag <- getByte
    case tag of
        0  -> return Null
        3  -> do
            len <- getVarInt
            bs <- getByteString $ fromIntegral len
            return $ StrNode $ BSC.unpack bs
        4  -> DoubleNode <$> getDouble
        -- 5  -> CharNode
        6  -> do
            i <- getVarInt
            return $ IntNode $ zzd i
        7  -> do
            arity <- getVarInt
            ps <- replicateM (fromIntegral arity) getNode
            return $ Tuple ps
        -- Literals contain a single literal node
        10 -> getNode
        11 -> Noun <$> getNoun
        12 -> BindingNode <$> getNoun
        13 -> do
            Tuple ns <- getNode
            return $ Sequence ns
        14 -> Call <$> getNode <*> getNode <*> getNode
        15 -> Def <$> getPattern <*> getNode <*> getNode
        16 -> Escape <$> getPattern <*> getNode <*> getPattern <*> getNode
        17 -> Object <$> getNode <*> getPattern <*> getNode <*> getNode
        18 -> do
            s <- Script <$> getNode
            Tuple methods <- getNode
            Tuple matchers <- getNode
            return $ s methods matchers
        19 -> do
            m <- Method <$> getNode <*> getNode
            ps <- getPatternList
            m ps <$> getNode <*> getNode
        20 -> Matcher <$> getPattern <*> getNode
        21 -> Assign <$> getNoun <*> getNode
        22 -> Finally <$> getNode <*> getNode
        23 -> Try <$> getNode <*> getPattern <*> getNode
        24 -> Hide <$> getNode
        25 -> If <$> getNode <*> getNode <*> getNode
        33 -> do
            StrNode [c] <- getNode
            return $ CharNode c
        x  -> fail $ "Not a node: " ++ show x
