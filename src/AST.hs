module AST where

type Identifier = String

data Expr = EVar Identifier
          | ENumber Double
          | EString String
          | EBinOp BinOp Expr Expr
          | EUnOp UnOp Expr
          | ECall Identifier [Expr]
          deriving (Eq, Show)

data BinOp = Add | Sub | Mul | Div | And | Or | Lt | Gt | Le | Ge | Eq | Ne
  deriving (Eq, Show)

data UnOp = Not | Minus | Plus
  deriving (Eq, Show)

data Stmt = SExpr Expr
          | SIf Expr Stmt Stmt
          | SWhile Expr Stmt
          | SFunction Identifier [Identifier] Block
          | SReturn Expr
          | SBlock Block
          | SAssn Identifier Expr
          deriving (Eq, Show)

type Block = [Stmt]
