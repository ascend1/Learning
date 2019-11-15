module Transformers
    ( eval0
    , eval2
    , runEval2
    , eval3
    , runEval3
    , eval4
    , runEval4
    , eval5
    , runEval5
    , eval6
    , runEval6
    , Name
    , Exp
    , Value
    , Env
    )
where

import           Control.Monad.Identity
import           Control.Monad.Trans.Error
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Maybe
import qualified Data.Map                      as Map

type Name = String  -- variable names

data Exp = Lit Integer -- expressions
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving(Show)

data Value = IntVal Integer -- values
           | FunVal Env Name Exp
           deriving(Show)

type Env = Map.Map Name Value-- mapping from names to values

-- base eval

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) =
    let IntVal i1 = eval0 env e1
        IntVal i2 = eval0 env e2
    in  IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) =
    let val1 = eval0 env e1
        val2 = eval0 env e2
    in  case val1 of
            FunVal env' n body -> eval0 (Map.insert n val2 env') body

-- eval1 being monadic
-- type Eval1 a = Identity a
-- This step does not work in GHC 8.6.5 since error handling is not default,
-- but the idea is as simple as lifting the evaluation result into an identity monad.

-- eval2 with ErrorT on top for error handling
type Eval2 a = ErrorT String Identity a
runEval2 :: Eval2 a -> Either String a
runEval2 = runIdentity . runErrorT

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
    Just v  -> return v
    Nothing -> throwError ("cannot resolve variable: " ++ n)
eval2 env (Plus e1 e2) = do
    e1' <- eval2 env e1
    e2' <- eval2 env e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> throwError "type error in addition"
eval2 env (Abs n  e ) = return $ FunVal env n e
eval2 env (App e1 e2) = do
    val1 <- eval2 env e1
    val2 <- eval2 env e2
    case val1 of
        FunVal env' n body -> eval2 (Map.insert n val2 env') body
        _                  -> throwError "type error in application"

-- eval3 with ReaderT on top for removing Env
-- note the usage of lift to encapsulate inner Monad with the outer layer
-- also the usage of local to modify the input r in the chain of Readers
type Eval3 a = ReaderT Env (ErrorT String Identity) a
runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env m = runIdentity . runErrorT $ runReaderT m env

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do
    env <- ask
    case Map.lookup n env of
        Just v  -> return v
        Nothing -> lift $ throwError ("cannot resolve variable: " ++ n)
eval3 (Plus e1 e2) = do
    e1' <- eval3 e1
    e2' <- eval3 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> lift $ throwError "type error in addition"
eval3 (Abs n e) = do
    env <- ask
    return $ FunVal env n e
eval3 (App e1 e2) = do
    val1 <- eval3 e1
    val2 <- eval3 e2
    case val1 of
        FunVal env' n body ->
            local (const (Map.insert n val2 env')) (eval3 body)
        _ -> lift $ throwError "type error in application"

-- eval4 with StateT to record the number of evalution steps
-- note that when using ErrorT and StateT, their order matters
type Eval4 a = ReaderT Env (ErrorT String (StateT Integer Identity)) a
runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env i m = runIdentity $ runStateT (runErrorT (runReaderT m env)) i

-- this function works because mtl package includes implemention
-- of making (ReaderT r m) as an instance of MonadState s (ReaderT r m)
tick :: (Num s, MonadState s m) => m ()
tick = do
    st <- get
    put (st + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = do
    tick
    return $ IntVal i
eval4 (Var n) = do
    tick
    env <- ask
    case Map.lookup n env of
        Just v  -> return v
        Nothing -> lift $ throwError ("cannot resolve variable: " ++ n)
eval4 (Plus e1 e2) = do
    tick
    e1' <- eval4 e1
    e2' <- eval4 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> lift $ throwError "type error in addition"
eval4 (Abs n e) = do
    tick
    env <- ask
    return $ FunVal env n e
eval4 (App e1 e2) = do
    tick
    val1 <- eval4 e1
    val2 <- eval4 e2
    case val1 of
        FunVal env' n body ->
            local (const (Map.insert n val2 env')) (eval4 body)
        _ -> lift $ throwError "type error in application"

-- eval5 with WriterT for logging
-- again, the order of WriterT and ErrorT matters
-- implementation here includes logs in WriteT in both success and failure cases
type Eval5 a
    = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer Identity))) a
runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env i m =
    runIdentity $ runStateT (runWriterT (runErrorT (runReaderT m env))) i

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = do
    tick
    return $ IntVal i
eval5 (Var n) = do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
        Just v  -> return v
        Nothing -> lift $ throwError ("cannot resolve variable: " ++ n)
eval5 (Plus e1 e2) = do
    tick
    e1' <- eval5 e1
    e2' <- eval5 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> lift $ throwError "type error in addition"
eval5 (Abs n e) = do
    tick
    env <- ask
    return $ FunVal env n e
eval5 (App e1 e2) = do
    tick
    val1 <- eval5 e1
    val2 <- eval5 e2
    case val1 of
        FunVal env' n body ->
            local (const (Map.insert n val2 env')) (eval5 body)
        _ -> lift $ throwError "type error in application"


-- eval6 with IO instead of Identity as the base monad
-- there is no monad transformer for IO, IO monad can only be used as a base monad
type Eval6 a
    = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer IO))) a
runEval6
    :: Env -> Integer -> Eval6 a -> IO ((Either String a, [String]), Integer)
runEval6 env i m = runStateT (runWriterT (runErrorT (runReaderT m env))) i

eval6 :: Exp -> Eval6 Value
eval6 (Lit i) = do
    tick
    liftIO $ print i -- OK because ReaderT, ErrorT, WriterT etc. are all instances of MonadIO
    return $ IntVal i
eval6 (Var n) = do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
        Just v  -> return v
        Nothing -> lift $ throwError ("cannot resolve variable: " ++ n)
eval6 (Plus e1 e2) = do
    tick
    e1' <- eval6 e1
    e2' <- eval6 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> lift $ throwError "type error in addition"
eval6 (Abs n e) = do
    tick
    env <- ask
    return $ FunVal env n e
eval6 (App e1 e2) = do
    tick
    val1 <- eval6 e1
    val2 <- eval6 e2
    case val1 of
        FunVal env' n body ->
            local (const (Map.insert n val2 env')) (eval6 body)
        _ -> lift $ throwError "type error in application"
