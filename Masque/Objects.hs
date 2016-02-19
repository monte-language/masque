{-# LANGUAGE TemplateHaskell #-}

module Masque.Objects where

import Control.Lens
import Data.Foldable (toList)
import Data.IORef
import Data.List
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Data.Unique

import Masque.AST

data Obj = NullObj
         | BoolObj Bool
         | CharObj Char
         | DoubleObj Double
         | IntObj Integer
         | StrObj String
         | EjectorObj Unique
         | ConstListObj (Seq.Seq Obj)
         | ConstMapObj [(Obj, Obj)]
         | UserObj Unique String String Env (M.Map String [(Patt, Expr)]) [(Patt, Expr)]

instance Show Obj where
    show NullObj = "null"
    show (BoolObj b) = if b then "true" else "false"
    show (CharObj c) = show c
    show (DoubleObj d) = show d
    show (IntObj i) = show i
    show (StrObj s) = show s
    show (EjectorObj _) = "<ejector>"
    show (ConstListObj objs) = "[" ++ intercalate "," (map show (toList objs)) ++ "]"
    show (ConstMapObj pairs) = let showPair (k, v) = show k ++ " => " ++ show v
        in "[" ++ intercalate "," (map showPair pairs) ++ "]"
    show (UserObj _ name _ _ _ _) = "<" ++ name ++ ">"

data Binding = FinalAnyBinding Obj
             | FinalBinding Obj Obj
             | VarAnyBinding (IORef Obj)
             | VarBinding (IORef Obj) Obj
             | FullBinding Obj

instance Show Binding where
    show (FinalAnyBinding _) = "<binding FinalSlot[Any]>"
    show (FinalBinding _ guard) = "<binding FinalSlot[" ++ show guard ++ "]>"
    show (VarAnyBinding _) = "<binding VarSlot[Any]>"
    show (VarBinding _ guard) = "<binding VarSlot[" ++ show guard ++ "]>"
    show (FullBinding binding) = show binding

newtype Env = Env { _unEnv :: M.Map String Binding }
    deriving (Show)

makeLenses ''Env
