{-# LANGUAGE LambdaCase #-}

module Interpreter (interpret) where

import Prelude hiding (print)
import AST
import Printer
import Text.Printf
import Text.Read (readMaybe)

import Data.Functor
import Control.Monad
import Control.Monad.State
import Control.Exception

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Value = VNumber Double
           | VString String
           | VBoolean Bool
           | VNull
           | VUndefined
           | VFunction Function String

data InterpreterExceptionType = InvalidCall 
                              | InvalidArgs

data InterpreterExceptionContext = InExpr Expr
                      | InFunction String
                      | InNativeCode String
                      | InTopLevel

data InterpreterException = InterpreterException InterpreterExceptionType [InterpreterExceptionContext]

instance Show InterpreterException where
  show (InterpreterException exc ctx) = (case exc of
      InvalidCall -> "ERROR! Attempt to call uncallable value\n"
      InvalidArgs -> "ERROR! Invalid number of arguments\n"
    ) <> (unlines $ flip map ctx $ \case
      InExpr expr       -> "  in expression " <> printExpr expr
      InFunction name   -> "  in function " <> show name
      InNativeCode name -> "  in function [native " <> show name <> "]"
      InTopLevel        -> "<end of stack trace>"
         )
  

instance Exception InterpreterException

data InterpreterState = InterpreterState { vars :: Map Identifier Value
                                         , ctxs :: [InterpreterExceptionContext]
                                         }

initialState :: InterpreterState
initialState = InterpreterState mempty [InTopLevel]


type Interpreter = StateT InterpreterState IO

type Args = [Value]
type Function = Args -> Interpreter Value

instance Show Value where
  show (VNumber n)      = show n
  show (VString s)      = s
  show (VBoolean True)  = "true"
  show (VBoolean False) = "false"
  show VNull            = "null"
  show VUndefined       = "undefined"
  show (VFunction _ s)  = s

getVar :: Identifier -> Interpreter Value
getVar name = gets (Map.lookup name . vars) >>= \case
  Just var -> return var
  Nothing -> return VUndefined

setVar :: Identifier -> Value -> Interpreter ()
setVar name value = modify (\s -> s { vars = Map.insert name value (vars s)})

native :: Identifier -> Function -> Interpreter ()
native name function = setVar name (VFunction fun repr)
  where repr = printf "function %s() { [native code] }" name
        fun args = do
          pushContext $ InNativeCode name
          retval <- function args
          popContext
          return retval

exception :: InterpreterExceptionType -> Interpreter a
exception exc = do
  ctx <- gets ctxs
  liftIO $ throwIO $ InterpreterException exc ctx

invalidCall :: Interpreter a
invalidCall = exception InvalidCall

invalidArgs :: Interpreter a
invalidArgs = exception InvalidArgs

toString :: Function
toString [x] = return $ VString $ show x
toString _ = invalidArgs

toNumber :: Function
toNumber [VNumber n     ] = return $ VNumber n
toNumber [VBoolean False] = return $ VNumber 0
toNumber [VBoolean True ] = return $ VNumber 1
toNumber [VNull         ] = return $ VNumber 0
toNumber [VString s     ] = return $ VNumber $ case readMaybe s of
                                     Just n -> n
                                     Nothing -> nan
toNumber [_]           = return $ VNumber nan
toNumber _             = invalidArgs

toBoolean :: Function
toBoolean [VNumber 0 ] = return $ VBoolean False
toBoolean [VBoolean b] = return $ VBoolean b
toBoolean [VString ""] = return $ VBoolean False
toBoolean [VNull     ] = return $ VBoolean False
toBoolean [VUndefined] = return $ VBoolean False
toBoolean [_]          = return $ VBoolean True
toBoolean _            = invalidArgs

print :: Function
print args = do
  liftIO (putStrLn $ unwords $ map show args)
  return VUndefined

input :: Function
input [] = do
  s <- liftIO getLine
  return $ VString s
input _ = invalidArgs

prelude :: Interpreter ()
prelude = do
  native "toString" toString
  native "toNumber" toNumber
  native "toBoolean" toBoolean
  native "print" print
  native "input" input
  setVar "true" (VBoolean True)
  setVar "false" (VBoolean False)
  setVar "null" VNull
  setVar "undefined" VUndefined

convert :: BinOp -> Function
convert Add = toNumber
convert Sub = toNumber
convert Mul = toNumber
convert Div = toNumber
convert And = toBoolean
convert Or  = toBoolean
convert _   = toString

binOpNumbers _ = invalidArgs

nan :: Double
nan = 0/0

binOp :: BinOp -> Function
binOp Add [VString  x , y          ] = return $ VString  (x <> show y)
binOp Add [x          , VString  y ] = return $ VString  (show x <> y)
binOp Add [VNumber  x , VNumber  y ] = return $ VNumber  (x + y)
binOp Sub [VNumber  x , VNumber  y ] = return $ VNumber  (x - y)
binOp Mul [VNumber  x , VNumber  y ] = return $ VNumber  (x * y)
binOp Div [VNumber  x , VNumber  y ] = return $ VNumber  (x / y)
binOp And [VBoolean x , VBoolean y ] = return $ VBoolean (x && y)
binOp Or  [VBoolean x , VBoolean y ] = return $ VBoolean (x || y)
binOp Lt  [VNumber  x , VNumber  y ] = return $ VBoolean (x < y)
binOp Lt  [VString  x , VString  y ] = return $ VBoolean (x < y)
binOp Lt  [VBoolean x , VBoolean y ] = return $ VBoolean (x < y)
binOp Eq  [VNumber  x , VNumber  y ] = return $ VBoolean (x == y)
binOp Eq  [VString  x , VString  y ] = return $ VBoolean (x == y)
binOp Eq  [VBoolean x , VBoolean y ] = return $ VBoolean (x == y)
binOp Eq  [VNull      , VNull      ] = return $ VBoolean True
binOp Eq  [VUndefined, VUndefined  ] = return $ VBoolean True
binOp Eq  [_         , _           ] = return $ VBoolean False
binOp Gt [x, y] = binOp Lt [y, x]
binOp Le [x, y] = binOp Gt [x, y] >>= (unOp Not . pure)
binOp Ge [x, y] = binOp Le [y, x]
binOp Ne [x, y] = binOp Eq [x, y] >>= (unOp Not . pure)

binOp op [x, y] = do
  a <- convert op [x]
  b <- convert op [y]
  binOp op [a, b]

binOp _ _ = invalidArgs

unOp :: UnOp -> Function
unOp Not [VBoolean b] = return $ VBoolean (not b)
unOp Not [x]       = do
  b <- toBoolean [x]
  unOp Not [b]
unOp Plus  [x]     = toNumber [x]
unOp Minus [x]     = binOp Sub [VNumber 0, x]

unOp _ _ = invalidArgs

pushContext :: InterpreterExceptionContext -> Interpreter ()
pushContext ctx = modify (\s -> s { ctxs = ctx:(ctxs s) })

popContext :: Interpreter ()
popContext = modify (\s -> s { ctxs = tail (ctxs s) })

callValue :: Value -> [Value] -> Interpreter Value
callValue (VFunction fun _) args = fun args
callValue _ _ = invalidCall

evalExpr :: Expr -> Interpreter Value
evalExpr expr = do
  pushContext (InExpr expr)
  val <- case expr of
    EVar name -> getVar name
    ENumber n -> return $ VNumber n
    EString s -> return $ VString s
    EBinOp op a b -> do
      a' <- evalExpr a
      b' <- evalExpr b
      binOp op [a', b']
    EUnOp op a -> do
      a' <- evalExpr a
      unOp op [a']
    ECall name args -> do
      args' <- traverse evalExpr args
      function <- getVar name
      callValue function args'
  popContext
  return val

evalStmt :: Stmt -> Interpreter (Maybe Value)
evalStmt (SExpr expr) = evalExpr expr $> Nothing
evalStmt (SIf expr body bodyElse) = do
  expr' <- evalExpr expr
  (VBoolean cond) <- toBoolean [expr']
  if cond then evalStmt body
          else evalStmt bodyElse
evalStmt (SWhile expr body) = do
  expr' <- evalExpr expr
  (VBoolean cond) <- toBoolean [expr']
  if cond then evalStmt body >>= \case
                  Nothing -> evalStmt (SWhile expr body)
                  Just retval -> return $ Just retval
          else return Nothing
evalStmt (SReturn expr) = Just <$> evalExpr expr
evalStmt (SBlock []) = return Nothing
evalStmt (SBlock (x:xs)) = evalStmt x >>= \case
                           Nothing -> evalStmt (SBlock xs)
                           Just retval -> return $ Just retval
evalStmt (SAssn name expr) = do
  expr' <- evalExpr expr
  setVar name expr'
  return Nothing
evalStmt (SFunction name args body) =
  let repr = printf "function %s() %s" name (printStmt (SBlock body))
      fun = \argv -> do
        when (length args /= length argv) invalidArgs
        old <- get
        pushContext $ InFunction name
        forM_ (zip args argv) (uncurry setVar)
        retval <- evalStmt (SBlock body) >>= \case
          Nothing -> return VUndefined
          Just retval -> return retval
        put old
        return retval
 in setVar name (VFunction fun repr) $> Nothing

interpret :: Block -> IO ()
interpret prog = void $ evalStateT (void $ prelude >> evalStmt (SBlock prog)) initialState
