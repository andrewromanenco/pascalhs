-- | Console interface to pascal interpreter.
-- Prints tokens, abstract syntax tree or interprets a program.

module Main where

import System.Environment

import Lexer
import Parser
import Interpreter


main :: IO ()
main = do
  args <- getArgs
  case args of
    ("lex":path:_) -> runLexer path
    ("ast":path:_) -> runParser path
    ("run":path:_) -> runInterpreter path
    (path:_) -> runInterpreter path
    _ -> putStrLn "Params: [lex|ast|run] path_to_pascal"

runLexer :: String -> IO ()
runLexer path = do
  sourceCode <- readFile path
  putStrLn (show (tokenize sourceCode))

runParser :: String -> IO ()
runParser path = do
  sourceCode <- readFile path
  putStrLn (show (parse (tokenize sourceCode)))

runInterpreter :: String -> IO ()
runInterpreter path = do
  sourceCode <- readFile path
  interpret (parse (tokenize sourceCode))
