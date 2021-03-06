module Masque.Monte where

import Control.Lens
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import Data.Foldable (toList)
import Data.IORef
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
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
         | UserObj Unique String Env (M.Map String [(Patt, Expr)]) [(Patt, Expr)]
         | Obj (String -> [Obj] -> [(Obj, Obj)] -> Monte Obj)

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
    show (UserObj _ name _ _ _) = "<" ++ name ++ ">"
    show (Obj _) = "<object>"

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

unEnv :: Lens' Env (M.Map String Binding)
unEnv f (Env m) = Env <$> f m

data Err = Refused
         | WrongType
         | Unsettled
         | Ejecting Unique Obj
         | Exception Obj
         | BadWrite String (S.Set String)
         | BadName String (S.Set String)

instance Show Err where
    show Refused = "Refused"
    show WrongType = "WrongType"
    show Unsettled = "Unsettled"
    show (Ejecting _ _) = "Ejecting"
    show (Exception _) = "Exception"
    show (BadWrite name _) = "BadWrite " ++ show name
    show (BadName name _) = "BadName " ++ show name


data MonteState = MS { _envStack :: NonEmpty Env
                     , _currentVat :: Vat }
    deriving (Show)

envStack :: Lens' MonteState (NonEmpty Env)
envStack f (MS env vat) = (`MS` vat) <$> f env

currentVat :: Lens' MonteState Vat
currentVat f (MS env vat) = MS env <$> f vat

-- | The monad of actions taken by Monte objects in an environment, resulting
--   in either objects or errors.
type Monte = EitherT Err (StateT MonteState IO)

-- | Run a Monte computation with an environment, producing the components of
--   the computation in `IO`.
runMonte :: Monte a -> NonEmpty Env -> IO (Either Err a, MonteState)
runMonte action envs = runStateT (runEitherT action) (MS envs (Vat [] "someVat"))

-- | Run a Monte computation with an empty environment.
runPureMonte :: Monte a -> IO (Either Err a, MonteState)
runPureMonte action = runMonte action (Env M.empty :| [])

-- | Refuse to handle a message. Typical reasons of refusal include not having
--   a method which corresponds to the message, not having any applicable
--   matchers, etc.
--
--   For handling wrong types in input values, such as when unwrapping, use
--   `WrongType` or one of its wrappers.
refuse :: Monte a
refuse = left Refused

-- | "It's the wrong type, Gromit!" ~ Wallace, implementing Monte
wrongType :: Monte a
wrongType = left WrongType

unwrapBool :: Obj -> Monte Bool
unwrapBool (BoolObj b) = return b
unwrapBool _ = wrongType

unwrapChar :: Obj -> Monte Char
unwrapChar (CharObj c) = return c
unwrapChar _ = wrongType

unwrapDouble :: Obj -> Monte Double
unwrapDouble (DoubleObj d) = return d
unwrapDouble _ = wrongType

-- | Like `unwrapDouble`, but also promotes Ints to Doubles. Useful for doing
--   some sorts of maths.
coerceDouble :: Obj -> Monte Double
coerceDouble (DoubleObj d) = return d
coerceDouble (IntObj i) = return $ fromIntegral i
coerceDouble _ = wrongType

unwrapInt :: Obj -> Monte Integer
unwrapInt (IntObj i) = return i
unwrapInt _ = wrongType

unwrapStr :: Obj -> Monte String
unwrapStr (StrObj s) = return s
unwrapStr _ = wrongType

-- | Wrap a Bool in a Monte object.
wrapBool :: Bool -> Monte Obj
wrapBool = return . BoolObj

-- | Wrap a Char in a Monte object.
wrapChar :: Char -> Monte Obj
wrapChar = return . CharObj

-- | Wrap a Double in a Monte object.
wrapDouble :: Double -> Monte Obj
wrapDouble = return . DoubleObj

-- | Wrap an Int in a Monte object.
wrapInt :: Integer -> Monte Obj
wrapInt = return . IntObj

-- | Wrap a Str in a Monte object.
wrapStr :: String -> Monte Obj
wrapStr = return . StrObj

namesInScope :: Monte (S.Set String)
namesInScope = do
    envs <- use envStack
    return $ S.unions (map (M.keysSet . _unEnv) (toList envs))

-- | Run a Monte action within a fresh scope. Names defined by the action will
--   be discarded afterwards.
withFreshScope :: Monte a -> Monte a
withFreshScope action =
    bracketEitherT push pop (const action)
    where
    push :: Monte ()
    push = envStack %= (Env M.empty NE.<|)
    -- Only works as long as the environment stack isn't overpopped during the
    -- scoped action. Shouldn't happen.
    pop :: () -> Monte ()
    pop _ = envStack %= (\(_ :| (a:as)) -> a :| as)

-- | Lookup a name in the current scope.
maybeLookupName :: String -> Monte (Maybe Binding)
maybeLookupName name = preuse $ envStack . traverse . unEnv . ix name

-- | Lookup a name in the current scope. If not found, transition to an
--   erroring state with a helpful error message.
lookupName :: String -> Monte Binding
lookupName name = do
    maybeBinding <- maybeLookupName name
    case maybeBinding of
        Just binding -> return binding
        Nothing -> do
            names <- namesInScope
            left $ BadName name names

-- XXX That empty () is for resolvers, once we have resolvers.
data Vat = Vat { _vatQueue :: [(Obj, String, [Obj], [(Obj, Obj)], ())]
               , _vatName :: String }
    deriving (Show)

vatQueue :: Lens' Vat [(Obj, String, [Obj], [(Obj, Obj)], ())]
vatQueue f (Vat q n) = (`Vat` n) <$> f q

vatName :: Lens' Vat String
vatName f (Vat q n) = Vat q <$> f n
