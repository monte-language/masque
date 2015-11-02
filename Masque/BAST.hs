{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Masque.BAST where

import Prelude hiding (mapM, sequence)
import Bound
import Bound.Name
import Bound.Scope
import Bound.Var
import Data.Foldable
import Data.List
import Data.Traversable
import Prelude.Extras

import Masque.AST

type BScope = Scope (Name String Int)
type BVar = Var (Name String Int) String

data BExpr a = BNullExpr
             | BCharExpr Char
             | BDoubleExpr Double
             | BIntExpr Integer
             | BStrExpr String
             | BBindingExpr a
             | BNounExpr a
             | BCallExpr (BExpr a) String [BExpr a] [(BExpr a, BExpr a)]
             | BLetExpr !Int (BPatt BExpr a) (BExpr a) (BExpr a) (BScope BExpr a)
             | BEscapeOnlyExpr (BPatt BExpr a) (BScope BExpr a)
             | BEscapeExpr (BPatt BExpr a) (BScope BExpr a) (BPatt BExpr a) (BScope BExpr a)
             | BFinallyExpr (BExpr a) (BExpr a)
             | BIfExpr (BExpr a) (BExpr a) (BExpr a)
             | BMatcherExpr (BPatt BExpr a) (BScope BExpr a)
             | BMethodExpr String String [BPatt BExpr a] [(BExpr a, BPatt BExpr a, BExpr a)] (BExpr a) (BScope BExpr a)
             | BObjectExpr String (BPatt BExpr a) (BExpr a) [BExpr a] [BExpr a] [BExpr a]
             | BSequenceExpr [BExpr a]
             | BTryExpr (BExpr a) (BPatt BExpr a) (BScope BExpr a)
    deriving (Eq, Foldable, Functor, Read, Show, Traversable)

instance Monad BExpr where
    return = BNounExpr

    BNullExpr >>= _ = BNullExpr
    BCharExpr c >>= _ = BCharExpr c
    BDoubleExpr d >>= _ = BDoubleExpr d
    BIntExpr i >>= _ = BIntExpr i
    BStrExpr s >>= _ = BStrExpr s
    BBindingExpr a >>= f = f a
    BNounExpr a >>= f = f a
    BCallExpr receiver verb args namedArgs >>= f = BCallExpr (receiver >>= f) verb (map (>>= f) args) (map (\(k, v) -> (k >>= f, v >>= f)) namedArgs)
    BLetExpr i patt exit rhs expr >>= f = BLetExpr i (patt >>>= f) (exit >>= f) (rhs >>= f) (expr >>>= f)
    BEscapeOnlyExpr patt expr >>= f = BEscapeOnlyExpr (patt >>>= f) (expr >>>= f)
    BEscapeExpr patt expr catchPatt catchExpr >>= f = BEscapeExpr (patt >>>= f) (expr >>>= f) (catchPatt >>>= f) (catchExpr >>>= f)
    BFinallyExpr expr finallyExpr >>= f = BFinallyExpr (expr >>= f) (finallyExpr >>= f)
    BIfExpr expr thenExpr elseExpr >>= f = BIfExpr (expr >>= f) (thenExpr >>= f) (elseExpr >>= f)
    BMatcherExpr patt expr >>= f = BMatcherExpr (patt >>>= f) (expr >>>= f)
    BMethodExpr doc verb patts namedPatts guard expr >>= f = BMethodExpr doc verb (map (>>>= f) patts) (map (\(k, p, v) -> (k >>= f, p >>>= f, v >>= f)) namedPatts) (guard >>= f) (expr >>>= f)
    BObjectExpr doc patt as implements methods matchers >>= f = BObjectExpr doc (patt >>>= f) (as >>= f) (map (>>= f) implements) (map (>>= f) methods) (map (>>= f) matchers)
    BSequenceExpr exprs >>= f = BSequenceExpr (map (>>= f) exprs)
    BTryExpr expr patt catch >>= f = BTryExpr (expr >>= f) (patt >>>= f) (catch >>>= f)

instance Eq1 BExpr
instance Read1 BExpr
instance Show1 BExpr

data BPatt f a = BIgnorePatt (BScope f a)
               | BBindPatt BVar
               | BFinalPatt BVar (BScope f a)
               | BListPatt [BPatt f a]
               | BVarPatt BVar (BScope f a)
               | BViaPatt (BScope f a) (BPatt f a)
    deriving (Eq, Foldable, Functor, Read, Show, Traversable)

instance Bound BPatt where
    BIgnorePatt guard >>>= f = BIgnorePatt (guard >>>= f)
    BBindPatt noun >>>= _ = BBindPatt noun
    BFinalPatt noun guard >>>= f = BFinalPatt noun (guard >>>= f)
    BListPatt patts >>>= f = BListPatt (map (>>>= f) patts)
    BVarPatt noun guard >>>= f = BVarPatt noun (guard >>>= f)
    BViaPatt via patt >>>= f = BViaPatt (via >>>= f) (patt >>>= f)

patternNames :: BPatt f a -> [String]
patternNames p = case p of
    BBindPatt n -> unvar (const []) return n
    BIgnorePatt _ -> []
    BFinalPatt n _ -> unvar (const []) return n
    BListPatt patts -> patts >>= patternNames
    BVarPatt n _ -> unvar (const []) return n
    BViaPatt _ patt -> patternNames patt

abstractNames :: [String] -> BExpr String -> BScope BExpr String
abstractNames names = abstract $ \name -> do
    i <- name `elemIndex` names
    return $ Name name i

abstractNothing :: BExpr a -> BScope BExpr a
abstractNothing = abstract $ const Nothing

bPatt :: Patt -> BPatt BExpr String
bPatt p = case p of
    BindPatt n -> BBindPatt (F n)
    IgnorePatt guard -> BIgnorePatt (abstractNothing $ bExpr guard)
    FinalPatt n guard -> BFinalPatt (F n) (abstractNothing $ bExpr guard)
    ListPatt patts -> BListPatt $ map bPatt patts
    VarPatt n guard -> BVarPatt (F n) (abstractNothing $ bExpr guard)

bExpr :: Expr -> BExpr String
bExpr e = case e of
    NullExpr -> BNullExpr
    NounExpr n -> BNounExpr n
    MatcherExpr patt expr -> let
        patt' = bPatt patt
        names = patternNames patt'
        in BMatcherExpr patt' (abstractNames names (bExpr expr))
