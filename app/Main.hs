{-# LANGUAGE LambdaCase #-}

module Main where

import Parser
import Printer
import Interpreter
import System.Environment

main :: IO ()
main = getArgs >>= \case
  [filename] -> parseProgram filename >>= interpret
  ["print", filename] -> parseProgram filename >>= (putStrLn . printProg)
  _ -> putStrLn "Usage: [print] <filename>"
