{-# LANGUAGE RecursiveDo #-}

-- | The actual evaluation strategies for anything resembling
--   `Expr -> Monte Obj`.
module Masque.Eval where

import Control.Lens
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Foldable (toList)
import Data.IORef
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Data.Unique

import Masque.AST
import Masque.Ejectors
import Masque.Monte
import Masque.Objects.Bool
import Masque.Objects.Double
import Masque.Objects.Int
import Masque.Objects.Str

-- | Smart constructor for a final binding; perform the guarding operation as
--   part of the assignment.
finalBinding :: Obj -> Obj -> Obj -> Monte Binding
finalBinding specimen guard ej = do
    specimen <- call guard "coerce" [specimen, ej] []
    return $ FinalBinding specimen guard

-- | Smart constructor for a var binding; perform the guarding operation as
--   part of the assignment.
varBinding :: Obj -> Obj -> Obj -> Monte Binding
varBinding specimen guard ej = do
    specimen <- call guard "coerce" [specimen, ej] []
    valueRef <- liftIO $ newIORef specimen
    return $ VarBinding valueRef guard

first :: Lens' (NonEmpty a) a
first = lens (\(a :| _) -> a) (\(_ :| as) -> (:| as))

-- | Bind a name to a binding within the current scope.
bindName :: String -> Binding -> Monte ()
bindName name binding = envStack . first . unEnv . at name ?= binding

-- | Unify a pattern with a specimen and ejector in the current scope.
unify :: Patt -> Obj -> Obj -> Monte ()
unify (BindPatt name) specimen _ = bindName name (FullBinding specimen)
unify (FinalPatt name NullExpr) specimen _ = bindName name (FinalAnyBinding specimen)
unify (FinalPatt name guardExpr) specimen ej = do
    guard <- eval guardExpr
    binding <- finalBinding specimen guard ej
    bindName name binding
unify (IgnorePatt NullExpr) _ _ = return ()
unify (IgnorePatt guardExpr) specimen ej = do
    guard <- eval guardExpr
    void $ call guard "coerce" [specimen, ej] []
unify (ListPatt patts) (ConstListObj specimens) ej
    | length patts == Seq.length specimens =
        void $ zipWithM_ (\patt specimen -> unify patt specimen ej) patts (toList specimens)
unify (ListPatt _) _ ej = throwEject ej (StrObj "List pattern failed")
unify (VarPatt name NullExpr) specimen _ = do
    valueRef <- liftIO $ newIORef specimen
    bindName name (VarAnyBinding valueRef)
unify (VarPatt name guardExpr) specimen ej = do
    guard <- eval guardExpr
    binding <- varBinding specimen guard ej
    bindName name binding
unify (ViaPatt transformExpr patt) specimen ej = do
    transformer <- eval transformExpr
    specimen' <- call transformer "run" [specimen, ej] []
    unify patt specimen' ej

-- | Evaluate a closed Monte expression in the current scope.
eval :: Expr -> Monte Obj
eval NullExpr = return NullObj
eval (CharExpr c) = return $ CharObj c
eval (DoubleExpr d) = return $ DoubleObj d
eval (IntExpr i) = return $ IntObj i
eval (StrExpr s) = return $ StrObj s
eval (AssignExpr name expr) = do
    rhs <- eval expr
    binding <- lookupName name
    case binding of
        FullBinding bindingObj -> do
            -- bindingObj.get().put(rhs)
            slot <- call bindingObj "get" [] []
            void $ call slot "put" [rhs] []
        VarBinding valueRef guard -> do
            specimen <- call guard "coerce" [rhs, NullObj] []
            liftIO $ writeIORef valueRef specimen
        VarAnyBinding valueRef -> liftIO $ writeIORef valueRef rhs
        _ -> do
            names <- namesInScope
            left $ BadWrite name names
    return rhs
eval (BindingExpr name) = do
    binding <- lookupName name
    return $ case binding of
        FullBinding bindingObj -> bindingObj
        _ -> error "eval: BindingExpr: Can't promote bindings yet"
eval (CallExpr objExpr verb argExprs namedArgExprs) = do
    obj <- eval objExpr
    args <- mapM eval argExprs
    namedArgs <- forM namedArgExprs $ \(NamedExpr k v) -> do
        key <- eval k
        value <- eval v
        return (key, value)
    call obj verb args namedArgs
eval (DefExpr patt ejExpr rhsExpr) = do
    rhs <- eval rhsExpr
    ej <- eval ejExpr
    unify patt rhs ej
    return rhs
eval (EscapeOnlyExpr patt expr) = do
    u <- liftIO newUnique
    catchEjector u (body u) return
    where
    body u = withFreshScope $ do
        unify patt (EjectorObj u) NullObj
        eval expr
eval (EscapeExpr patt expr catchPatt catchExpr) = do
    u <- liftIO newUnique
    catchEjector u (body u) catchBody
    where
    body u = withFreshScope $ do
        unify patt (EjectorObj u) NullObj
        eval expr
    catchBody problem = withFreshScope $ do
        unify catchPatt problem NullObj
        eval catchExpr
eval (FinallyExpr expr atLast) = bracketEitherT before after return
    where
    before = withFreshScope $ eval expr
    after obj = withFreshScope $ eval atLast >> return obj
eval (IfExpr condExpr consExpr altExpr) = withFreshScope $ do
    cond <- eval condExpr
    b <- unwrapBool cond
    eval $ if b then consExpr else altExpr
eval (HideExpr expr) = withFreshScope $ eval expr
eval (NounExpr name) = do
    binding <- lookupName name
    case binding of
        FullBinding bindingObj -> do
            -- bindingObj.get().get()
            slot <- call bindingObj "get" [] []
            call slot "get" [] []
        VarBinding valueRef _ -> liftIO $ readIORef valueRef
        VarAnyBinding valueRef -> liftIO $ readIORef valueRef
        FinalBinding value _ -> return value
        FinalAnyBinding value -> return value
eval (ObjectExpr _ patt asAuditor auditors methods matchers) = mdo
    -- Use recursive do to tie the self-referential knot of objects. We give
    -- the object a reference to its environment, but trust the user to not
    -- specify an object with ill-founded recursion in its pattern.
    u <- liftIO newUnique
    let rv = UserObj u objName env methodMap matcherList
    unify patt rv NullObj
    env <- uses envStack $ \es -> Env (M.unions (map _unEnv (NE.toList es)))
    return rv
    where
    methodMap = M.fromListWith (++) methodList
    -- We need more structured methods and matchers.
    methodList = [(verb, [(ListPatt p', n)]) | MethodExpr _ verb p' _ _ n <- methods ]
    matcherList = [(p', n) | MatcherExpr p' n <- matchers ]
    objName = case patt of
        FinalPatt name _ -> name
        VarPatt name _   -> "var " ++ name
        _                -> "_"
eval (SequenceExpr exprs) = do
    values <- mapM eval exprs 
    return $ if null exprs then NullObj else last values
eval (TryExpr expr catchPatt catchBody) = catchError try recover
    where
    try = withFreshScope $ eval expr
    -- This should eventually respect the information-hiding schemes that
    -- Typhon has adopted.
    recover ej@Ejecting{} = left ej
    recover _ = withFreshScope $ do
        unify catchPatt (StrObj "<placeholder problem>") NullObj
        eval catchBody

-- | Deliver a message immediately to an object.
call :: Obj -> String -> [Obj] -> [(Obj, Obj)] -> Monte Obj
call obj verb args namedArgs = case obj of
    BoolObj b   -> callBool b verb args namedArgs
    DoubleObj d -> callDouble d verb args namedArgs
    IntObj i    -> callInt i verb args namedArgs
    StrObj s    -> callStr s verb args namedArgs
    _           -> error "Not written yet"
