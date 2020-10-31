{-# LANGUAGE LambdaCase #-}

module Parser (parser, parseProgram) where

import AST
import System.Exit
import Data.Functor
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

type Parser = Parsec Void String

parseProgram :: FilePath -> IO Block
parseProgram filename = do
  f <- readFile filename
  case parse parser filename f of
    Left err -> do
      putStr $ errorBundlePretty err
      exitFailure
    Right block -> return block

parser :: Parser Block
parser = do
  prog <- parseStmtSequence
  eof
  return prog

keyword :: String -> Parser ()
keyword s = do
  string s
  notFollowedBy alphaNumChar
  sc

ident :: Parser String
ident = do
  firstChar <- letterChar
  rest <- many alphaNumChar
  sc
  return $ firstChar:rest

numberL :: Parser Double
numberL = L.signed sc (try L.float <|> L.decimal)

stringL :: Parser String
stringL = do
  char '"'
  manyTill L.charLiteral $ char '"'

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parseStmtSequence :: Parser Block
parseStmtSequence = many parseStmt

parseStmt :: Parser Stmt
parseStmt = choice [ parseSIf
                   , parseSWhile
                   , parseSFunction
                   , parseSReturn
                   , parseSBlock
                   , try parseSAssn
                   , parseSExpr
                   , symbol ";" $> SBlock []
                   ]

parseSIf :: Parser Stmt
parseSIf = do
  keyword "if"
  expr <- parens parseExpr
  body <- parseStmt
  bodyElse <- option (SBlock []) $ keyword "else" >> parseStmt
  return $ SIf expr body bodyElse

parseSWhile :: Parser Stmt
parseSWhile = do
  keyword "while"
  expr <- parens parseExpr
  body <- parseStmt
  return $ SWhile expr body

parseSFunction :: Parser Stmt
parseSFunction = do
  keyword "function"
  name <- ident
  args <- parens $ ident `sepBy` symbol ","
  body <- braces parseStmtSequence
  return $ SFunction name args body

parseSReturn :: Parser Stmt
parseSReturn = do
  keyword "return"
  expr <- parseExpr
  symbol ";"
  return $ SReturn expr

parseSBlock :: Parser Stmt
parseSBlock = do
  block <- braces parseStmtSequence
  return $ SBlock block

parseSAssn :: Parser Stmt
parseSAssn = do
  var <- ident
  symbol "="
  expr <- parseExpr
  symbol ";"
  return $ SAssn var expr

parseSExpr :: Parser Stmt
parseSExpr = do
  expr <- parseExpr
  symbol ";"
  return $ SExpr expr

parseExprTerm :: Parser Expr
parseExprTerm = choice [ parens parseExpr
                       , try parseFunctionCall
                       , EVar <$> ident
                       , ENumber <$> numberL
                       , EString <$> stringL
                       ]

parseFunctionCall :: Parser Expr
parseFunctionCall = do
  name <- ident
  args <- parens $ parseExpr `sepBy` symbol ","
  return $ ECall name args

parseExpr :: Parser Expr
parseExpr = makeExprParser parseExprTerm $
  [ [ prefix "-"  $ EUnOp Minus 
    , prefix "+"  $ EUnOp Plus  ]
  , [ prefix "!"  $ EUnOp Not   ]
  , [ binary "*"  $ EBinOp Mul  
    , binary "/"  $ EBinOp Div  ]
  , [ binary "+"  $ EBinOp Add  
    , binary "-"  $ EBinOp Sub  ]
  , [ binary "==" $ EBinOp Eq   
    , binary "!=" $ EBinOp Ne   
    , binary ">=" $ EBinOp Ge   
    , binary "<=" $ EBinOp Le   
    , binary ">"  $ EBinOp Gt   
    , binary "<"  $ EBinOp Lt   ]
  , [ binary "&&" $ EBinOp And  ]
  , [ binary "||" $ EBinOp Or   ]
  ]

binary name f = InfixL (f <$ symbol name)
prefix name f = Prefix (f <$ symbol name)
