module Main where

import Lexer
import Parser

main :: IO ()
main = do sourceCode <- readFile "manual_test.pas"
          putStrLn (show (parse (tokenize sourceCode)))
