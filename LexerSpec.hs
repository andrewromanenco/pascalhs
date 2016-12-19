module LexerSpec where

import Test.Hspec
import Lexer

main :: IO ()
main = hspec $ do
  describe "position in text" $ do
    it "line number" $
      lineNumber (Pos 1 2) `shouldBe` 1

    it "column number" $
      column (Pos 1 2) `shouldBe` 2

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

  describe "tokenize static" $ do
    it "AND" $
      nextStaticToken "and" `shouldBe` Just (AND, "and", "")

    it "ARRAY" $
      nextStaticToken "array" `shouldBe` Just (ARRAY, "array", "")

    it "BEGIN" $
      nextStaticToken "begin" `shouldBe` Just (BEGIN, "begin", "")

    it "BOOLEAN" $
      nextStaticToken "boolean" `shouldBe` Just (BOOLEAN, "boolean", "")

    it "CASE" $
      nextStaticToken "case" `shouldBe` Just (CASE, "case", "")

    it "CHAR" $
      nextStaticToken "char" `shouldBe` Just (CHAR, "char", "")

    it "CHR" $
      nextStaticToken "chr" `shouldBe` Just (CHR, "chr", "")

    it "CONST" $
      nextStaticToken "const" `shouldBe` Just (CONST, "const", "")

    it "DIV" $
      nextStaticToken "div" `shouldBe` Just (DIV, "div", "")

    it "DO" $
      nextStaticToken "do" `shouldBe` Just (DO, "do", "")

    it "DOWNTO" $
      nextStaticToken "downto" `shouldBe` Just (DOWNTO, "downto", "")

    it "ELSE" $
      nextStaticToken "else" `shouldBe` Just (ELSE, "else", "")

    it "END" $
      nextStaticToken "end" `shouldBe` Just (END, "end", "")

    it "FILE" $
      nextStaticToken "file" `shouldBe` Just (FILE, "file", "")

    it "FOR" $
      nextStaticToken "for" `shouldBe` Just (FOR, "for", "")

    it "FUNCTION" $
      nextStaticToken "function" `shouldBe` Just (FUNCTION, "function", "")

    it "GOTO" $
      nextStaticToken "goto" `shouldBe` Just (GOTO, "goto", "")

    it "IF" $
      nextStaticToken "if" `shouldBe` Just (IF, "if", "")

    it "IN" $
      nextStaticToken "in" `shouldBe` Just (IN, "in", "")

    it "INTEGER" $
      nextStaticToken "integer" `shouldBe` Just (INTEGER, "integer", "")

    it "LABEL" $
      nextStaticToken "label" `shouldBe` Just (LABEL, "label", "")

    it "MOD" $
      nextStaticToken "mod" `shouldBe` Just (MOD, "mod", "")

    it "NIL" $
      nextStaticToken "nil" `shouldBe` Just (NIL, "nil", "")

    it "NOT" $
      nextStaticToken "not" `shouldBe` Just (NOT, "not", "")

    it "OF" $
      nextStaticToken "of" `shouldBe` Just (OF, "of", "")

    it "OR" $
      nextStaticToken "or" `shouldBe` Just (OR, "or", "")

    it "PACKED" $
      nextStaticToken "packed" `shouldBe` Just (PACKED, "packed", "")

    it "PROCEDURE" $
      nextStaticToken "procedure" `shouldBe` Just (PROCEDURE, "procedure", "")

    it "PROGRAM" $
      nextStaticToken "program" `shouldBe` Just (PROGRAM, "program", "")

    it "REAL" $
      nextStaticToken "real" `shouldBe` Just (REAL, "real", "")

    it "RECORD" $
      nextStaticToken "record" `shouldBe` Just (RECORD, "record", "")

    it "REPEAT" $
      nextStaticToken "repeat" `shouldBe` Just (REPEAT, "repeat", "")

    it "SET" $
      nextStaticToken "set" `shouldBe` Just (SET, "set", "")

    it "THEN" $
      nextStaticToken "then" `shouldBe` Just (THEN, "then", "")

    it "TO" $
      nextStaticToken "to" `shouldBe` Just (TO, "to", "")

    it "TYPE" $
      nextStaticToken "type" `shouldBe` Just (TYPE, "type", "")

    it "UNTIL" $
      nextStaticToken "until" `shouldBe` Just (UNTIL, "until", "")

    it "VAR" $
      nextStaticToken "var" `shouldBe` Just (VAR, "var", "")

    it "WHILE" $
      nextStaticToken "while" `shouldBe` Just (WHILE, "while", "")

    it "WITH" $
      nextStaticToken "with" `shouldBe` Just (WITH, "with", "")

    it "PLUS" $
      nextStaticToken "+" `shouldBe` Just (PLUS, "+", "")

    it "MINUS" $
      nextStaticToken "-" `shouldBe` Just (MINUS, "-", "")

    it "STAR" $
      nextStaticToken "*" `shouldBe` Just (STAR, "*", "")

    it "SLASH" $
      nextStaticToken "/" `shouldBe` Just (SLASH, "/", "")

    it "ASSIGN" $
      nextStaticToken ":=" `shouldBe` Just (ASSIGN, ":=", "")

    it "COMMA" $
      nextStaticToken "," `shouldBe` Just (COMMA, ",", "")

    it "SEMI" $
      nextStaticToken ";" `shouldBe` Just (SEMI, ";", "")

    it "COLON" $
      nextStaticToken ":" `shouldBe` Just (COLON, ":", "")

    it "EQUAL" $
      nextStaticToken "=" `shouldBe` Just (EQUAL, "=", "")

    it "NOT_EQUAL" $
      nextStaticToken "<>" `shouldBe` Just (NOT_EQUAL, "<>", "")

    it "LT" $
      nextStaticToken "<" `shouldBe` Just (LT_, "<", "")

    it "LE" $
      nextStaticToken "<=" `shouldBe` Just (LE, "<=", "")

    it "GE" $
      nextStaticToken ">=" `shouldBe` Just (GE, ">=", "")

    it "GT" $
      nextStaticToken ">" `shouldBe` Just (GT_, ">", "")

    it "LPAREN" $
      nextStaticToken "(" `shouldBe` Just (LPAREN, "(", "")

    it "RPAREN" $
      nextStaticToken ")" `shouldBe` Just (RPAREN, ")", "")

    it "LBRACK" $
      nextStaticToken "[" `shouldBe` Just (LBRACK, "[", "")

    it "LBRACK2" $
      nextStaticToken "(." `shouldBe` Just (LBRACK2, "(.", "")

    it "RBRACK" $
      nextStaticToken "]" `shouldBe` Just (RBRACK, "]", "")

    it "RBRACK2" $
      nextStaticToken ".)" `shouldBe` Just (RBRACK2, ".)", "")

    it "POINTER" $
      nextStaticToken "^" `shouldBe` Just (POINTER, "^", "")

    it "AT" $
      nextStaticToken "@" `shouldBe` Just (AT, "@", "")

    it "DOT" $
      nextStaticToken "." `shouldBe` Just (DOT, ".", "")

    it "DOTDOT" $
      nextStaticToken ".." `shouldBe` Just (DOTDOT, "..", "")

    it "LCURLY" $
      nextStaticToken "{" `shouldBe` Just (LCURLY, "{", "")

    it "RCURLY" $
      nextStaticToken "}" `shouldBe` Just (RCURLY, "}", "")

    it "UNIT" $
      nextStaticToken "unit" `shouldBe` Just (UNIT, "unit", "")

    it "INTERFACE" $
      nextStaticToken "interface" `shouldBe` Just (INTERFACE, "interface", "")

    it "USES" $
      nextStaticToken "uses" `shouldBe` Just (USES, "uses", "")

    it "STRING" $
      nextStaticToken "string" `shouldBe` Just (STRING, "string", "")

    it "IMPLEMENTATION" $
      nextStaticToken "implementation" `shouldBe` Just (IMPLEMENTATION, "implementation", "")

  describe "tokenize dynamic" $ do
    it "WS" $
      nextDynamicToken "  \t  \r\n " `shouldBe` Just (WS "  \t  \r\n ", "")

    it "Comment1 single line" $
      nextDynamicToken "(*some comment*)" `shouldBe` Just (COMMENT_1 "(*some comment*)", "")

    it "Comment1 multiple lines" $
      nextDynamicToken "(*some\ncomment*)" `shouldBe` Just (COMMENT_1 "(*some\ncomment*)", "")

    it "Comment2 single line" $
      nextDynamicToken "{some comment}" `shouldBe` Just (COMMENT_2 "{some comment}", "")

    it "Comment2 multiple lines" $
      nextDynamicToken "{some\ncomment}" `shouldBe` Just (COMMENT_2 "{some\ncomment}", "")

    it "STRING_LITERAL str" $
      nextDynamicToken "'str'" `shouldBe` Just (STRING_LITERAL "'str'", "")

    it "STRING_LITERAL empty" $
      nextDynamicToken "''" `shouldBe` Just (STRING_LITERAL "''", "")

    it "STRING_LITERAL with ' inside'" $
      nextDynamicToken "'abc''cba'" `shouldBe` Just (STRING_LITERAL "'abc''cba'", "")

    it "NUM_INT 123" $
      nextDynamicToken "123" `shouldBe` Just (NUM_INT "123", "")

    it "NUM_INT 123.34" $
      nextDynamicToken "123.34" `shouldBe` Just (NUM_INT "123.34", "")

    it "NUM_INT 123e-34" $
      nextDynamicToken "123e-34" `shouldBe` Just (NUM_INT "123e-34", "")

    it "NUM_INT 123.45e56" $
      nextDynamicToken "123.45e56" `shouldBe` Just (NUM_INT "123.45e56", "")

    it "IDENT x" $
      nextDynamicToken "x" `shouldBe` Just (IDENT "x", "")

    it "IDENT xA9_u" $
      nextDynamicToken "xA9_u" `shouldBe` Just (IDENT "xA9_u", "")

    it "IDENT Abc1" $
      nextDynamicToken "Abc1" `shouldBe` Just (IDENT "Abc1", "")

  describe "nextToken" $ do
    it "IDEN wins" $
      nextToken "ANDi" `shouldBe` Just (IDENT "ANDi", "ANDi", "")

    it "AND wins" $
      nextToken "AND" `shouldBe` Just (AND, "AND", "")

  describe "tokenize input to list" $ do
    it "empty" $
      tokenize "" `shouldBe` [EOF]

    it "expression" $
      tokenize "if x<y then a:=45" `shouldBe` [IF, WS " ", IDENT "x", LT_,IDENT "y",WS " ", THEN, WS " ", IDENT "a", ASSIGN, NUM_INT "45", EOF]

  describe "tokenize samples" $ do
    it "subscripts.pas" $ do
      input <- readFile "res/examples/subscripts.pas"
      tokenize input `shouldEndWith` [EOF]

    it "set.pas" $ do
      input <- readFile "res/examples/set.pas"
      tokenize input `shouldEndWith` [EOF]

    it "pointer.pas" $ do
      input <- readFile "res/examples/pointer.pas"
      tokenize input `shouldEndWith` [EOF]

    it "passfail.pas" $ do
      input <- readFile "res/examples/passfail.pas"
      tokenize input `shouldEndWith` [EOF]

    it "nesting.pas" $ do
      input <- readFile "res/examples/nesting.pas"
      tokenize input `shouldEndWith` [EOF]

    it "linkedlist2.pas" $ do
      input <- readFile "res/examples/linkedlist2.pas"
      tokenize input `shouldEndWith` [EOF]

    it "if.pas" $ do
      input <- readFile "res/examples/if.pas"
      tokenize input `shouldEndWith` [EOF]

    it "helloworld.pas" $ do
      input <- readFile "res/examples/helloworld.pas"
      tokenize input `shouldEndWith` [EOF]

    it "fact.pas" $ do
      input <- readFile "res/examples/fact.pas"
      tokenize input `shouldEndWith` [EOF]

    it "case.pas" $ do
      input <- readFile "res/examples/case.pas"
      tokenize input `shouldEndWith` [EOF]

    it "bubble.pas" $ do
      input <- readFile "res/examples/bubble.pas"
      tokenize input `shouldEndWith` [EOF]

    it "array2.pas" $ do
      input <- readFile "res/examples/array2.pas"
      tokenize input `shouldEndWith` [EOF]

    it "array.pas" $ do
      input <- readFile "res/examples/array.pas"
      tokenize input `shouldEndWith` [EOF]

    it "add.pas" $ do
      input <- readFile "res/examples/add.pas"
      tokenize input `shouldEndWith` [EOF]
