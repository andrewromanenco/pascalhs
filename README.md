# Pascal interpreter in Haskell
Pascal grammar is defined for ANTLR: https://github.com/antlr/grammars-v4/tree/master/pascal and a copy is available in res folder.

This is my pet project to learn haskell and it does not cover 100% of pascal grammar. But it can execute HelloWorld.pas.


## How to run

 - To execute a program: runhaskell Pascal.hs res/examples/helloworld.pas
 - To view abstract syntax tree: runhaskell Pascal.hs ast res/examples/helloworld.pas
 - To view tokens: runhaskell Pascal.hs lex res/examples/helloworld.pas


## Project structure
Project implements standard modules: lexer, parser and interpreter.

LexerTokens.hs - all tokens defined in grammar.<br/>
LexerRules.hs - all rules to read specific tokens from an input stream. This step implemented via regex matching (which is slow).<br/>
Lexer.hs - actual code to provide tokens from an input.<br/>

AST.hs - abstract syntax tree definitions.<br/>
Parser.hs - parser rules.<br/>

Interpreter.hs - executes a given abstract syntax tree.

Pascal.hs - command line interface.


## Tests
Tests use Hspec. To run manually: runhaskell test/NAME_OF_TEST_CASE_FILE
