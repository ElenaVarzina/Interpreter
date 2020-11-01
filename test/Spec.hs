import Test.HUnit
import System.IO.Silently
import Text.Megaparsec
import Control.Monad

import AST
import Parser
import Printer
import Interpreter

source = "n = 10.0;\n\
\i = 1.0;\n\
\r = 1.0;\n\
\while (i <= n) {\n\
\    r = r * i;\n\
\    i = i + 1.0;\n\
\}\n\
\\n\
\print(n,\"factorial\",\"equals\",r);\n"

ast = [ SAssn "n" (ENumber 10)
      , SAssn "i" (ENumber 1)
      , SAssn "r" (ENumber 1)
      , SWhile (EBinOp Le (EVar "i") (EVar "n")) (SBlock [
          SAssn "r" (EBinOp Mul (EVar "r") (EVar "i")) ,
          SAssn "i" (EBinOp Add (EVar "i") (ENumber 1))
        ])
      , SExpr (ECall "print" [EVar "n", EString "factorial", EString "equals", EVar "r"])
      ]

testParser :: Test
testParser = TestCase $ assertEqual "parser" (Right ast) (parse parser "" source)

testPrinter :: Test
testPrinter = TestCase $ assertEqual "printer" source (printProg ast)

testInterpreter :: Test
testInterpreter = TestCase $ do
  out <- capture_ $ interpret ast
  assertEqual "interpreter" out "10.0 factorial equals 3628800.0\n"

main :: IO ()
main = void $ runTestTT $ TestList [ testParser, testPrinter, testInterpreter ]
