# Pascal interpreter in Haskell

Work-in-progress.

This project may become an interpreter for Pascal.

Pascal grammar is defined for ANTLR: https://github.com/antlr/grammars-v4/tree/master/pascal

## Lexer
Lexer rules are copied from ANTLR grammar (see in res/pascal.g4) and are using list of regex instead of building a proper automation. This is slow by design.

## Haskell
The project is my first ever Haskell code. 

## Test
Tests use Hspec. To run manually: runhaskell LexerSpec.hs
