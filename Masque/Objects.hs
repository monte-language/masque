{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Masque.Objects where

import Data.Foldable (toList)
import Data.IORef
import Data.List
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Data.Unique
import System.IO

import Masque.AST

data Obj = NullObj
         | BoolObj Bool
         | CharObj Char
         | DoubleObj Double
         | IntObj Integer
         | StrObj String
         | EjectorObj Unique
         | BindingObj
         | RefObj (IORef (Maybe Obj))
         | ResObj (IORef (Maybe Obj))
         | FountObj Handle (IORef Obj)
         | DrainObj Handle
         | ConstListObj (Seq.Seq Obj)
         | ConstMapObj [(Obj, Obj)]
         | BuiltinObj String
         | UserObj Unique String Env (M.Map String [(Pattern, Node)]) [(Pattern, Node)]

instance Show Obj where
    show NullObj = "null"
    show (BoolObj b) = if b then "true" else "false"
    show (CharObj c) = show c
    show (DoubleObj d) = show d
    show (IntObj i) = show i
    show (StrObj s) = show s
    show (EjectorObj _) = "<ejector>"
    show BindingObj = "<binding>"
    show (RefObj _) = "<ref>"
    show (ResObj _) = "<resolver>"
    show (FountObj _ _) = "<fount>"
    show (DrainObj _) = "<drain>"
    show (ConstListObj objs) = "[" ++ intercalate "," (map show (toList objs)) ++ "]"
    show (ConstMapObj pairs) = let showPair (k, v) = show k ++ " => " ++ show v
        in "[" ++ intercalate "," (map showPair pairs) ++ "]"
    show (BuiltinObj name) = "<" ++ name ++ ">"
    show (UserObj _ name _ _ _) = "<" ++ name ++ ">"

data Binding = DefBind Obj
             | VarBind { _bSlot, _bGuard :: IORef Obj }

instance Show Binding where
    show (DefBind o) = "DefBind " ++ show o
    show (VarBind _ _) = "VarBind ..."

newtype Env = Env { _unEnv :: M.Map String Binding }
    deriving (Show)
