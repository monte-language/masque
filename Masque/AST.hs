module Masque.AST where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Bits
import qualified Data.ByteString.Char8 as BSC
import Data.Foldable
import Data.List (genericIndex)

data Expr = NullExpr
          | CharExpr Char
          | DoubleExpr Double
          | IntExpr Integer
          | StrExpr String
          | AssignExpr String Expr
          | BindingExpr String
          | CallExpr Expr String [Expr] [NamedExpr]
          | DefExpr Patt Expr Expr
          | EscapeOnlyExpr Patt Expr
          | EscapeExpr Patt Expr Patt Expr
          | FinallyExpr Expr Expr
          | HideExpr Expr
          | IfExpr Expr Expr Expr
          | MatcherExpr Patt Expr
          | MethodExpr String String [Patt] [NamedPatt] Expr Expr
          | NounExpr String
          | ObjectExpr String Patt Expr [Expr] [Expr] [Expr]
          | SequenceExpr [Expr]
          | TryExpr Expr Patt Expr
    deriving (Eq, Show, Read)

data NamedExpr = NamedExpr Expr Expr
    deriving (Eq, Show, Read)

data Patt = IgnorePatt Expr
          | BindPatt String
          | FinalPatt String Expr
          | ListPatt [Patt]
          | VarPatt String Expr
          | ViaPatt Expr Patt
    deriving (Eq, Show, Read)

data NamedPatt = NamedPatt Expr Patt Expr
    deriving (Eq, Show, Read)

-- | Deserialization

type MASTContext = ([Expr], [Patt])

getVarInt :: StateT MASTContext Get Integer
getVarInt = do
    b <- lift getWord8
    let rv = toInteger $ b .&. 0x7f
    if b .&. 0x80 == 0x00
        then return rv
        else do
            vi <- getVarInt
            return $ rv .|. vi `shiftL` 7

getDouble :: StateT MASTContext Get Double
getDouble = do
    bs <- replicateM 8 (lift getWord8)
    let w64 = foldl' (\w x -> w `shiftL` 8 .|. fromIntegral x) 0 bs
    return $ wordToDouble w64

zzd :: Integer -> Integer
zzd i = if i .&. 1 == 1 then (i `div` 2) `xor` (-1) else i `div` 2

-- XXX UTF-8 decode
getStr :: StateT MASTContext Get String
getStr = do
    len <- getVarInt
    bs <- lift $ getByteString (fromIntegral len)
    return $ BSC.unpack bs

getExpr :: StateT MASTContext Get Expr
getExpr = do
    i <- getVarInt
    gets (\(exprs, _) -> exprs `genericIndex` i)

getExprs :: StateT MASTContext Get [Expr]
getExprs = do
    i <- getVarInt
    replicateM (fromInteger i) getExpr

getNamedExprs :: StateT MASTContext Get [NamedExpr]
getNamedExprs = do
    i <- getVarInt
    replicateM (fromInteger i) $ do
        k <- getExpr
        v <- getExpr
        return $ NamedExpr k v

getPatt :: StateT MASTContext Get Patt
getPatt = do
    i <- getVarInt
    gets (\(_, patts) -> patts `genericIndex` i)

getPatts :: StateT MASTContext Get [Patt]
getPatts = do
    i <- getVarInt
    replicateM (fromInteger i) getPatt

getNamedPatts :: StateT MASTContext Get [NamedPatt]
getNamedPatts = do
    i <- getVarInt
    replicateM (fromInteger i) $ do
        k <- getExpr
        p <- getPatt
        v <- getExpr
        return $ NamedPatt k p v

nextTag :: StateT MASTContext Get ()
nextTag = do
    tag <- lift getWord8
    case (toEnum . fromEnum) tag of
        -- Literals
        'L' -> do
            literalTag <- lift getWord8
            expr <- case (toEnum . fromEnum) literalTag of
                -- 'C' -> do ...
                'D' -> DoubleExpr <$> getDouble
                'I' -> do
                    i <- getVarInt
                    return $ IntExpr $ zzd i
                'N' -> return NullExpr
                'S' -> StrExpr <$> getStr
            modify (\(exprs, patts) -> (exprs ++ [expr], patts))
        -- Patts
        'P' -> do
            pattTag <- lift getWord8
            patt <- case (toEnum . fromEnum) pattTag of
                'A' -> ViaPatt <$> getExpr <*> getPatt
                'B' -> BindPatt <$> getStr
                'F' -> FinalPatt <$> getStr <*> getExpr
                'I' -> IgnorePatt <$> getExpr
                'L' -> ListPatt <$> getPatts
                'V' -> VarPatt <$> getStr <*> getExpr
            modify (\(exprs, patts) -> (exprs, patts ++ [patt]))
        -- Exprs
        tag -> do
            expr <- case tag of
                'A' -> AssignExpr <$> getStr <*> getExpr
                'B' -> BindingExpr <$> getStr
                'C' -> CallExpr <$> getExpr <*> getStr <*> getExprs <*> getNamedExprs
                'D' -> DefExpr <$> getPatt <*> getExpr <*> getExpr
                'E' -> EscapeExpr <$> getPatt <*> getExpr <*> getPatt <*> getExpr
                'F' -> FinallyExpr <$> getExpr <*> getExpr
                'H' -> HideExpr <$> getExpr
                'I' -> IfExpr <$> getExpr <*> getExpr <*> getExpr
                'M' -> MethodExpr <$> getStr <*> getStr <*> getPatts <*> getNamedPatts <*> getExpr <*> getExpr
                'N' -> NounExpr <$> getStr
                'O' -> ObjectExpr <$> getStr <*> getPatt <*> getExpr <*> getExprs <*> getExprs <*> getExprs
                'R' -> MatcherExpr <$> getPatt <*> getExpr
                'S' -> SequenceExpr <$> getExprs
                'Y' -> TryExpr <$> getExpr <*> getPatt <*> getExpr
                'e' -> EscapeOnlyExpr <$> getPatt <*> getExpr
            modify (\(exprs, patts) -> (exprs ++ [expr], patts))
