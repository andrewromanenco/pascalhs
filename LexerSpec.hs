module LexerSpec where

import Test.Hspec
import Lexer

main :: IO ()
main = hspec $ do
  describe "stripExistingPrefix" $ do
    it "curs off existing prefix" $
      stripExistingPrefix "pre" "prefix" `shouldBe` "fix"

  describe "match_token" $ do
    it "matches pattern to the front" $
      match_token "[0-9]+" "123abc" `shouldBe` Just ("123", "abc")

    it "does not matches if not in the front" $
      match_token "999" "x999abc" `shouldBe` Nothing

    it "match is case insensitive" $
      match_token "abc" "aBc123" `shouldBe` Just ("aBc", "123")

  describe "tokenize chain" $ do
    it "several tokens" $
      tokenize "and array" `shouldBe` [AND, WS " ", ARRAY, EOF]

  describe "tokenize" $ do
    it "EOF" $
      tokenize "" `shouldBe` [EOF]

    it "AND" $
      tokenize "and" `shouldBe` [AND, EOF]

    it "ARRAY" $
      tokenize "array" `shouldBe` [ARRAY, EOF]

    it "BEGIN" $
      tokenize "begin" `shouldBe` [BEGIN, EOF]

    it "BOOLEAN" $
      tokenize "boolean" `shouldBe` [BOOLEAN, EOF]

    it "CASE" $
      tokenize "case" `shouldBe` [CASE, EOF]

    it "CHAR" $
      tokenize "char" `shouldBe` [CHAR, EOF]

    it "CHR" $
      tokenize "chr" `shouldBe` [CHR, EOF]

    it "CONST" $
      tokenize "const" `shouldBe` [CONST, EOF]

    it "DIV" $
      tokenize "div" `shouldBe` [DIV, EOF]

    it "DO" $
      tokenize "do" `shouldBe` [DO, EOF]

    it "DOWNTO" $
      tokenize "downto" `shouldBe` [DOWNTO, EOF]

    it "ELSE" $
      tokenize "else" `shouldBe` [ELSE, EOF]

    it "END" $
      tokenize "end" `shouldBe` [END, EOF]

    it "FILE" $
      tokenize "file" `shouldBe` [FILE, EOF]

    it "FOR" $
      tokenize "for" `shouldBe` [FOR, EOF]

    it "FUNCTION" $
      tokenize "function" `shouldBe` [FUNCTION, EOF]

    it "GOTO" $
      tokenize "goto" `shouldBe` [GOTO, EOF]

    it "IF" $
      tokenize "if" `shouldBe` [IF, EOF]

    it "IN" $
      tokenize "in" `shouldBe` [IN, EOF]

    it "INTEGER" $
      tokenize "integer" `shouldBe` [INTEGER, EOF]

    it "LABEL" $
      tokenize "label" `shouldBe` [LABEL, EOF]

    it "MOD" $
      tokenize "mod" `shouldBe` [MOD, EOF]

    it "NIL" $
      tokenize "nil" `shouldBe` [NIL, EOF]

    it "NOT" $
      tokenize "not" `shouldBe` [NOT, EOF]

    it "OF" $
      tokenize "of" `shouldBe` [OF, EOF]

    it "OR" $
      tokenize "or" `shouldBe` [OR, EOF]

    it "PACKED" $
      tokenize "packed" `shouldBe` [PACKED, EOF]

    it "PROCEDURE" $
      tokenize "procedure" `shouldBe` [PROCEDURE, EOF]

    it "PROGRAM" $
      tokenize "program" `shouldBe` [PROGRAM, EOF]

    it "REAL" $
      tokenize "real" `shouldBe` [REAL, EOF]

    it "RECORD" $
      tokenize "record" `shouldBe` [RECORD, EOF]

    it "REPEAT" $
      tokenize "repeat" `shouldBe` [REPEAT, EOF]

    it "SET" $
      tokenize "set" `shouldBe` [SET, EOF]

    it "THEN" $
      tokenize "then" `shouldBe` [THEN, EOF]

    it "TO" $
      tokenize "to" `shouldBe` [TO, EOF]

    it "TYPE" $
      tokenize "type" `shouldBe` [TYPE, EOF]

    it "UNTIL" $
      tokenize "until" `shouldBe` [UNTIL, EOF]

    it "VAR" $
      tokenize "var" `shouldBe` [VAR, EOF]

    it "WHILE" $
      tokenize "while" `shouldBe` [WHILE, EOF]

    it "WITH" $
      tokenize "with" `shouldBe` [WITH, EOF]

    it "PLUS" $
      tokenize "+" `shouldBe` [PLUS, EOF]

    it "MINUS" $
      tokenize "-" `shouldBe` [MINUS, EOF]

    it "STAR" $
      tokenize "*" `shouldBe` [STAR, EOF]

    it "SLASH" $
      tokenize "/" `shouldBe` [SLASH, EOF]

    it "ASSIGN" $
      tokenize ":=" `shouldBe` [ASSIGN, EOF]

    it "COMMA" $
      tokenize "," `shouldBe` [COMMA, EOF]

    it "SEMI" $
      tokenize ";" `shouldBe` [SEMI, EOF]

    it "COLON" $
      tokenize ":" `shouldBe` [COLON, EOF]

    it "EQUAL" $
      tokenize "=" `shouldBe` [EQUAL, EOF]

    it "NOT_EQUAL" $
      tokenize "<>" `shouldBe` [NOT_EQUAL, EOF]

    it "LT" $
      tokenize "<" `shouldBe` [LT_, EOF]

    it "LE" $
      tokenize "<=" `shouldBe` [LE, EOF]

    it "GE" $
      tokenize ">=" `shouldBe` [GE, EOF]

    it "GT" $
      tokenize ">" `shouldBe` [GT_, EOF]

    it "LPAREN" $
      tokenize "(" `shouldBe` [LPAREN, EOF]

    it "RPAREN" $
      tokenize ")" `shouldBe` [RPAREN, EOF]

    it "LBRACK" $
      tokenize "[" `shouldBe` [LBRACK, EOF]

    it "LBRACK2" $
      tokenize "(." `shouldBe` [LBRACK2, EOF]

    it "RBRACK" $
      tokenize "]" `shouldBe` [RBRACK, EOF]

    it "RBRACK2" $
      tokenize ".)" `shouldBe` [RBRACK2, EOF]

    it "POINTER" $
      tokenize "^" `shouldBe` [POINTER, EOF]

    it "AT" $
      tokenize "@" `shouldBe` [AT, EOF]

    it "DOT" $
      tokenize "." `shouldBe` [DOT, EOF]

    it "DOTDOT" $
      tokenize ".." `shouldBe` [DOTDOT, EOF]

    it "LCURLY" $
      tokenize "{" `shouldBe` [LCURLY, EOF]

    it "RCURLY" $
      tokenize "}" `shouldBe` [RCURLY, EOF]

    it "UNIT" $
      tokenize "unit" `shouldBe` [UNIT, EOF]

    it "INTERFACE" $
      tokenize "interface" `shouldBe` [INTERFACE, EOF]

    it "USES" $
      tokenize "uses" `shouldBe` [USES, EOF]

    it "STRING" $
      tokenize "string" `shouldBe` [STRING, EOF]

    it "IMPLEMENTATION" $
      tokenize "implementation" `shouldBe` [IMPLEMENTATION, EOF]

    it "WS" $
      tokenize "  \t  \r\n " `shouldBe` [WS "  \t  \r\n ", EOF]

    it "Comment1 single line" $
      tokenize "(*some comment*)" `shouldBe` [COMMENT_1 "(*some comment*)", EOF]

    it "Comment1 multiple lines" $
      tokenize "(*some\ncomment*)" `shouldBe` [COMMENT_1 "(*some\ncomment*)", EOF]

    it "Comment2 single line" $
      tokenize "{some comment}" `shouldBe` [COMMENT_2 "{some comment}", EOF]

    it "Comment2 multiple lines" $
      tokenize "{some\ncomment}" `shouldBe` [COMMENT_2 "{some\ncomment}", EOF]
