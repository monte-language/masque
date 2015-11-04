{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Masque.BAST where

import Prelude hiding (mapM, sequence)
import Bound
import Bound.Name
import Bound.Scope
import Bound.Var
import Control.Applicative
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
             | BLetExpr (BExpr a) (BExpr a) !Int (BPatt BExpr a) (BScope BExpr a)
             | BEscapeOnlyExpr !Int (BPatt BExpr a) (BScope BExpr a)
             | BEscapeExpr !Int (BPatt BExpr a) (BScope BExpr a) !Int (BPatt BExpr a) (BScope BExpr a)
             | BFinallyExpr (BExpr a) (BExpr a)
             | BIfExpr (BExpr a) (BExpr a) (BExpr a)
             | BMatcherExpr !Int (BPatt BExpr a) (BScope BExpr a)
             | BMethodExpr String String [BPatt BExpr a] [(BExpr a, BPatt BExpr a, BExpr a)] (BExpr a) (BScope BExpr a)
             | BObjectExpr String (BPatt BExpr a) (BExpr a) [BExpr a] [BExpr a] [BExpr a]
             | BSequenceExpr [BExpr a]
             | BTryExpr (BExpr a) !Int (BPatt BExpr a) (BScope BExpr a)
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
    BLetExpr exit rhs i patt expr >>= f = BLetExpr (exit >>= f) (rhs >>= f) i (patt >>>= f) (expr >>>= f)
    BEscapeOnlyExpr i patt expr >>= f = BEscapeOnlyExpr i (patt >>>= f) (expr >>>= f)
    BEscapeExpr i patt expr i' catchPatt catchExpr >>= f = BEscapeExpr i (patt >>>= f) (expr >>>= f) i' (catchPatt >>>= f) (catchExpr >>>= f)
    BFinallyExpr expr finallyExpr >>= f = BFinallyExpr (expr >>= f) (finallyExpr >>= f)
    BIfExpr expr thenExpr elseExpr >>= f = BIfExpr (expr >>= f) (thenExpr >>= f) (elseExpr >>= f)
    BMatcherExpr i patt expr >>= f = BMatcherExpr i (patt >>>= f) (expr >>>= f)
    BMethodExpr doc verb patts namedPatts guard expr >>= f = BMethodExpr doc verb (map (>>>= f) patts) (map (\(k, p, v) -> (k >>= f, p >>>= f, v >>= f)) namedPatts) (guard >>= f) (expr >>>= f)
    BObjectExpr doc patt as implements methods matchers >>= f = BObjectExpr doc (patt >>>= f) (as >>= f) (map (>>= f) implements) (map (>>= f) methods) (map (>>= f) matchers)
    BSequenceExpr exprs >>= f = BSequenceExpr (map (>>= f) exprs)
    BTryExpr expr i patt catch >>= f = BTryExpr (expr >>= f) i (patt >>>= f) (catch >>>= f)

instance Eq1 BExpr
instance Read1 BExpr
instance Show1 BExpr

data BPatt f a = BIgnorePatt (BScope f a)
               | BBindPatt
               | BFinalPatt (BScope f a)
               | BListPatt [BPatt f a]
               | BVarPatt (BScope f a)
               | BViaPatt (BScope f a) (BPatt f a)
    deriving (Eq, Foldable, Functor, Read, Show, Traversable)

instance Bound BPatt where
    BIgnorePatt guard >>>= f = BIgnorePatt (guard >>>= f)
    BBindPatt >>>= _ = BBindPatt
    BFinalPatt guard >>>= f = BFinalPatt (guard >>>= f)
    BListPatt patts >>>= f = BListPatt (map (>>>= f) patts)
    BVarPatt guard >>>= f = BVarPatt (guard >>>= f)
    BViaPatt via patt >>>= f = BViaPatt (via >>>= f) (patt >>>= f)

data BP a = BP { bppatt :: [a] -> BPatt BExpr a, bpbinds :: [a] }

abstractNothing :: BExpr a -> BScope BExpr a
abstractNothing = abstract $ const Nothing

bPatt :: Patt -> BP String
bPatt p = case p of
    BindPatt n -> BP (const BBindPatt) [n]
    IgnorePatt g -> BP (const (BIgnorePatt (bScope g))) []
    FinalPatt n g -> BP (const (BFinalPatt (bScope g))) [n]
    -- XXX tie the knot for things like _suchThat
    ListPatt ps -> let bps = map bPatt ps
        in BP (\outer -> BListPatt (map bppatt bps <*> pure outer)) (bps >>= bpbinds)
    VarPatt n g -> BP (const (BVarPatt (bScope g))) [n]
    ViaPatt via patt -> let BP p' inner = bPatt patt
        in BP (\outer -> BViaPatt (abstractNames outer (bExpr via)) (p' outer)) inner
    where
    bScope = abstractNothing . bExpr

abstractNames :: [String] -> BExpr String -> BScope BExpr String
abstractNames names = abstract $ \name -> do
    i <- name `elemIndex` names
    return $ Name name i

bExpr :: Expr -> BExpr String
bExpr e = case e of
    NullExpr -> BNullExpr
    CharExpr c -> BCharExpr c
    DoubleExpr d -> BDoubleExpr d
    IntExpr i -> BIntExpr i
    StrExpr s -> BStrExpr s
    BindingExpr n -> BBindingExpr n
    NounExpr n -> BNounExpr n
    CallExpr receiver verb args namedArgs -> BCallExpr (bExpr receiver) verb (map bExpr args) (map (\(NamedExpr k v) -> (bExpr k, bExpr v)) namedArgs)
    EscapeOnlyExpr patt expr -> buildExpr BEscapeOnlyExpr patt expr
    EscapeExpr patt expr patt' expr' -> buildExpr (buildExpr BEscapeExpr patt expr) patt' expr'
    FinallyExpr try finally -> BFinallyExpr (bExpr try) (bExpr finally)
    IfExpr test cons alt -> BIfExpr (bExpr test) (bExpr cons) (bExpr alt)
    MatcherExpr patt expr -> buildExpr BMatcherExpr patt expr
    -- Defs in sequences become lets.
    SequenceExpr [] -> BNullExpr
    SequenceExpr (DefExpr patt exit expr:es) -> let builder = BLetExpr (bExpr exit) (bExpr expr)
        in buildExpr builder patt $ SequenceExpr es
    SequenceExpr (expr:[]) -> bExpr expr
    -- Try harder than normal to not create spurious sequences.
    SequenceExpr (expr:es) -> case bExpr (SequenceExpr es) of
        BSequenceExpr bes -> BSequenceExpr (be:bes)
        tail -> BSequenceExpr [be, tail]
        where
        be = bExpr expr
    TryExpr try patt catch -> buildExpr (BTryExpr (bExpr try)) patt catch
    DefExpr{} -> error "Bare def"
    where
    buildExpr builder patt expr = case bPatt patt of
        BP p bs -> builder (length bs) (p []) (abstractNames bs (bExpr expr))

-- BMethodExpr String String [BPatt BExpr a] [(BExpr a, BPatt BExpr a, BExpr a)] (BExpr a) (BScope BExpr a)
-- BObjectExpr String (BPatt BExpr a) (BExpr a) [BExpr a] [BExpr a] [BExpr a]
