module Printer (printExpr, printStmt, printProg) where

import AST
import Text.Printf
import Data.List

indent :: String -> String
indent s = intercalate "\n" $ map ("    " <>) (lines s)

binOpStr :: BinOp -> String
binOpStr Add = "+"
binOpStr Sub = "-"
binOpStr Mul = "*"
binOpStr Div = "/"
binOpStr And = "&&"
binOpStr Or  = "||"
binOpStr Gt  = ">"
binOpStr Ge  = ">="
binOpStr Lt  = "<"
binOpStr Le  = "<="
binOpStr Eq  = "=="
binOpStr Ne  = "!="

unOpStr :: UnOp -> String
unOpStr Not = "not "
unOpStr Minus = "-"
unOpStr Plus = "-"

binOpPrec :: BinOp -> Int
binOpPrec Add = 6
binOpPrec Sub = 6
binOpPrec Mul = 7
binOpPrec Div = 7
binOpPrec And = 3
binOpPrec Or  = 3
binOpPrec Lt  = 5
binOpPrec Le  = 5
binOpPrec Gt  = 5
binOpPrec Ge  = 5
binOpPrec Eq  = 4
binOpPrec Ne  = 4

parens :: Bool -> String -> String
parens False s = s
parens True s = "(" <> s <> ")"

printExpr :: Expr -> String
printExpr = printExprPrec 0

printExprPrec :: Int -> Expr -> String
printExprPrec prec (EVar name)       = name
printExprPrec prec (ENumber n)       = show n
printExprPrec prec (EString s)       = show s
printExprPrec prec (EBinOp op a b)   = parens (prec >= binOpPrec op) $ printf "%s %s %s" (printExprPrec (binOpPrec op) a) (binOpStr op) (printExprPrec (binOpPrec op) b)
printExprPrec prec (EUnOp op a)      = parens (prec >= 8) $ printf "%s%s" (unOpStr op) (printExprPrec 8 a)
printExprPrec prec (ECall name args) = printf "%s(%s)" name (intercalate "," (map printExpr args))

printStmt :: Stmt -> String
printStmt (SExpr expr)                = printf "%s;" (printExpr expr)
printStmt (SIf expr body (SBlock [])) = printf "if (%s) %s" (printExpr expr) (printStmt body)
printStmt (SIf expr body bodyElse)    = printf "if (%s) %s else %s" (printExpr expr) (printStmt body) (printStmt bodyElse)
printStmt (SWhile expr body)          = printf "while (%s) %s" (printExpr expr) (printStmt body)
printStmt (SFunction name args body)  = printf "function %s(%s) %s" name (intercalate "," args) (printStmt (SBlock body))
printStmt (SReturn expr)              = printf "return %s;" (printExpr expr)
printStmt (SAssn name expr)           = printf "%s = %s;" name (printExpr expr)
printStmt (SBlock block)              = "{\n" <> unlines (map (indent . printStmt) block) <> "}\n"

printProg :: Block -> String
printProg stmts = unlines $ map printStmt stmts
