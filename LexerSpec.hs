module LexerSpec where

import Test.Hspec
import Lexer

main :: IO ()
main = hspec $ do
  describe "position in text" $ do
    it "line number" $
      lineNumber (Cursor 1 2) `shouldBe` 1

    it "column number" $
      column (Cursor 1 2) `shouldBe` 2

  describe "move position" $ do
    it "no move on empty" $
      moveCursor (Cursor 1 2) "" `shouldBe` Cursor 1 2

    it "move left" $
      moveCursor (Cursor 1 2) "abc" `shouldBe` Cursor 1 5

    it "move left ignoring \\r" $
      moveCursor (Cursor 1 2) "a\rt" `shouldBe` Cursor 1 4

    it "move left and down" $
      moveCursor (Cursor 1 2) "abc\n" `shouldBe` Cursor 2 1

    it "move left down left" $
      moveCursor (Cursor 1 2) "abc\nabc" `shouldBe` Cursor 2 4

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
      nextStaticToken "and" (Cursor 0 1) `shouldBe` Just (AND (Cursor 0 1), "and", "")

    it "ARRAY" $
      nextStaticToken "array" (Cursor 0 1) `shouldBe` Just (ARRAY (Cursor 0 1), "array", "")

    it "BEGIN" $
      nextStaticToken "begin" (Cursor 0 1) `shouldBe` Just (BEGIN (Cursor 0 1), "begin", "")

    it "BOOLEAN" $
      nextStaticToken "boolean" (Cursor 0 1) `shouldBe` Just (BOOLEAN (Cursor 0 1), "boolean", "")

    it "CASE" $
      nextStaticToken "case" (Cursor 0 1) `shouldBe` Just (CASE (Cursor 0 1), "case", "")

    it "CHAR" $
      nextStaticToken "char" (Cursor 0 1) `shouldBe` Just (CHAR (Cursor 0 1), "char", "")

    it "CHR" $
      nextStaticToken "chr" (Cursor 0 1) `shouldBe` Just (CHR (Cursor 0 1), "chr", "")

    it "CONST" $
      nextStaticToken "const" (Cursor 0 1) `shouldBe` Just (CONST (Cursor 0 1), "const", "")

    it "DIV" $
      nextStaticToken "div" (Cursor 0 1) `shouldBe` Just (DIV (Cursor 0 1), "div", "")

    it "DO" $
      nextStaticToken "do" (Cursor 0 1) `shouldBe` Just (DO (Cursor 0 1), "do", "")

    it "DOWNTO" $
      nextStaticToken "downto" (Cursor 0 1) `shouldBe` Just (DOWNTO (Cursor 0 1), "downto", "")

    it "ELSE" $
      nextStaticToken "else" (Cursor 0 1) `shouldBe` Just (ELSE (Cursor 0 1), "else", "")

    it "END" $
      nextStaticToken "end" (Cursor 0 1) `shouldBe` Just (END (Cursor 0 1), "end", "")

    it "FILE" $
      nextStaticToken "file" (Cursor 0 1) `shouldBe` Just (FILE (Cursor 0 1), "file", "")

    it "FOR" $
      nextStaticToken "for" (Cursor 0 1) `shouldBe` Just (FOR (Cursor 0 1), "for", "")

    it "FUNCTION" $
      nextStaticToken "function" (Cursor 0 1) `shouldBe` Just (FUNCTION (Cursor 0 1), "function", "")

    it "GOTO" $
      nextStaticToken "goto" (Cursor 0 1) `shouldBe` Just (GOTO (Cursor 0 1), "goto", "")

    it "IF" $
      nextStaticToken "if" (Cursor 0 1) `shouldBe` Just (IF (Cursor 0 1), "if", "")

    it "IN" $
      nextStaticToken "in" (Cursor 0 1) `shouldBe` Just (IN (Cursor 0 1), "in", "")

    it "INTEGER" $
      nextStaticToken "integer" (Cursor 0 1) `shouldBe` Just (INTEGER (Cursor 0 1), "integer", "")

    it "LABEL" $
      nextStaticToken "label" (Cursor 0 1) `shouldBe` Just (LABEL (Cursor 0 1), "label", "")

    it "MOD" $
      nextStaticToken "mod" (Cursor 0 1) `shouldBe` Just (MOD (Cursor 0 1), "mod", "")

    it "NIL" $
      nextStaticToken "nil" (Cursor 0 1) `shouldBe` Just (NIL (Cursor 0 1), "nil", "")

    it "NOT" $
      nextStaticToken "not" (Cursor 0 1) `shouldBe` Just (NOT (Cursor 0 1), "not", "")

    it "OF" $
      nextStaticToken "of" (Cursor 0 1) `shouldBe` Just (OF (Cursor 0 1), "of", "")

    it "OR" $
      nextStaticToken "or" (Cursor 0 1) `shouldBe` Just (OR (Cursor 0 1), "or", "")

    it "PACKED" $
      nextStaticToken "packed" (Cursor 0 1) `shouldBe` Just (PACKED (Cursor 0 1), "packed", "")

    it "PROCEDURE" $
      nextStaticToken "procedure" (Cursor 0 1) `shouldBe` Just (PROCEDURE (Cursor 0 1), "procedure", "")

    it "PROGRAM" $
      nextStaticToken "program" (Cursor 0 1) `shouldBe` Just (PROGRAM (Cursor 0 1), "program", "")

    it "REAL" $
      nextStaticToken "real" (Cursor 0 1) `shouldBe` Just (REAL (Cursor 0 1), "real", "")

    it "RECORD" $
      nextStaticToken "record" (Cursor 0 1) `shouldBe` Just (RECORD (Cursor 0 1), "record", "")

    it "REPEAT" $
      nextStaticToken "repeat" (Cursor 0 1) `shouldBe` Just (REPEAT (Cursor 0 1), "repeat", "")

    it "SET" $
      nextStaticToken "set" (Cursor 0 1) `shouldBe` Just (SET (Cursor 0 1), "set", "")

    it "THEN" $
      nextStaticToken "then" (Cursor 0 1) `shouldBe` Just (THEN (Cursor 0 1), "then", "")

    it "TO" $
      nextStaticToken "to" (Cursor 0 1) `shouldBe` Just (TO (Cursor 0 1), "to", "")

    it "TYPE" $
      nextStaticToken "type" (Cursor 0 1) `shouldBe` Just (TYPE (Cursor 0 1), "type", "")

    it "UNTIL" $
      nextStaticToken "until" (Cursor 0 1) `shouldBe` Just (UNTIL (Cursor 0 1), "until", "")

    it "VAR" $
      nextStaticToken "var" (Cursor 0 1) `shouldBe` Just (VAR (Cursor 0 1), "var", "")

    it "WHILE" $
      nextStaticToken "while" (Cursor 0 1) `shouldBe` Just (WHILE (Cursor 0 1), "while", "")

    it "WITH" $
      nextStaticToken "with" (Cursor 0 1) `shouldBe` Just (WITH (Cursor 0 1), "with", "")

    it "PLUS" $
      nextStaticToken "+" (Cursor 0 1) `shouldBe` Just (PLUS (Cursor 0 1), "+", "")

    it "MINUS" $
      nextStaticToken "-" (Cursor 0 1) `shouldBe` Just (MINUS (Cursor 0 1), "-", "")

    it "STAR" $
      nextStaticToken "*" (Cursor 0 1) `shouldBe` Just (STAR (Cursor 0 1), "*", "")

    it "SLASH" $
      nextStaticToken "/" (Cursor 0 1) `shouldBe` Just (SLASH (Cursor 0 1), "/", "")

    it "ASSIGN" $
      nextStaticToken ":=" (Cursor 0 1) `shouldBe` Just (ASSIGN (Cursor 0 1), ":=", "")

    it "COMMA" $
      nextStaticToken "," (Cursor 0 1) `shouldBe` Just (COMMA (Cursor 0 1), ",", "")

    it "SEMI" $
      nextStaticToken ";" (Cursor 0 1) `shouldBe` Just (SEMI (Cursor 0 1), ";", "")

    it "COLON" $
      nextStaticToken ":" (Cursor 0 1) `shouldBe` Just (COLON (Cursor 0 1), ":", "")

    it "EQUAL" $
      nextStaticToken "=" (Cursor 0 1) `shouldBe` Just (EQUAL (Cursor 0 1), "=", "")

    it "NOT_EQUAL" $
      nextStaticToken "<>" (Cursor 0 1) `shouldBe` Just (NOT_EQUAL (Cursor 0 1), "<>", "")

    it "LT" $
      nextStaticToken "<" (Cursor 0 1) `shouldBe` Just (LT_ (Cursor 0 1), "<", "")

    it "LE" $
      nextStaticToken "<=" (Cursor 0 1) `shouldBe` Just (LE (Cursor 0 1), "<=", "")

    it "GE" $
      nextStaticToken ">=" (Cursor 0 1) `shouldBe` Just (GE (Cursor 0 1), ">=", "")

    it "GT" $
      nextStaticToken ">" (Cursor 0 1) `shouldBe` Just (GT_ (Cursor 0 1), ">", "")

    it "LPAREN" $
      nextStaticToken "(" (Cursor 0 1) `shouldBe` Just (LPAREN (Cursor 0 1), "(", "")

    it "RPAREN" $
      nextStaticToken ")" (Cursor 0 1) `shouldBe` Just (RPAREN (Cursor 0 1), ")", "")

    it "LBRACK" $
      nextStaticToken "[" (Cursor 0 1) `shouldBe` Just (LBRACK (Cursor 0 1), "[", "")

    it "LBRACK2" $
      nextStaticToken "(." (Cursor 0 1) `shouldBe` Just (LBRACK2 (Cursor 0 1), "(.", "")

    it "RBRACK" $
      nextStaticToken "]" (Cursor 0 1) `shouldBe` Just (RBRACK (Cursor 0 1), "]", "")

    it "RBRACK2" $
      nextStaticToken ".)" (Cursor 0 1) `shouldBe` Just (RBRACK2 (Cursor 0 1), ".)", "")

    it "POINTER" $
      nextStaticToken "^" (Cursor 0 1) `shouldBe` Just (POINTER (Cursor 0 1), "^", "")

    it "AT" $
      nextStaticToken "@" (Cursor 0 1) `shouldBe` Just (AT (Cursor 0 1), "@", "")

    it "DOT" $
      nextStaticToken "." (Cursor 0 1) `shouldBe` Just (DOT (Cursor 0 1), ".", "")

    it "DOTDOT" $
      nextStaticToken ".." (Cursor 0 1) `shouldBe` Just (DOTDOT (Cursor 0 1), "..", "")

    it "LCURLY" $
      nextStaticToken "{" (Cursor 0 1) `shouldBe` Just (LCURLY (Cursor 0 1), "{", "")

    it "RCURLY" $
      nextStaticToken "}" (Cursor 0 1) `shouldBe` Just (RCURLY (Cursor 0 1), "}", "")

    it "UNIT" $
      nextStaticToken "unit" (Cursor 0 1) `shouldBe` Just (UNIT (Cursor 0 1), "unit", "")

    it "INTERFACE" $
      nextStaticToken "interface" (Cursor 0 1) `shouldBe` Just (INTERFACE (Cursor 0 1), "interface", "")

    it "USES" $
      nextStaticToken "uses" (Cursor 0 1) `shouldBe` Just (USES (Cursor 0 1), "uses", "")

    it "STRING" $
      nextStaticToken "string" (Cursor 0 1) `shouldBe` Just (STRING (Cursor 0 1), "string", "")

    it "IMPLEMENTATION" $
      nextStaticToken "implementation" (Cursor 0 1) `shouldBe` Just (IMPLEMENTATION (Cursor 0 1), "implementation", "")

  describe "tokenize dynamic" $ do
    it "WS" $
      nextDynamicToken "  \t  \r\n " (Cursor 0 1) `shouldBe` Just (WS (Cursor 0 1) "  \t  \r\n ", "")

    it "Comment1 single line" $
      nextDynamicToken "(*some comment*)" (Cursor 0 1) `shouldBe` Just (COMMENT_1 (Cursor 0 1) "(*some comment*)", "")

    it "Comment1 multiple lines" $
      nextDynamicToken "(*some\ncomment*)" (Cursor 0 1) `shouldBe` Just (COMMENT_1 (Cursor 0 1) "(*some\ncomment*)", "")

    it "Comment2 single line" $
      nextDynamicToken "{some comment}" (Cursor 0 1) `shouldBe` Just (COMMENT_2 (Cursor 0 1) "{some comment}", "")

    it "Comment2 multiple lines" $
      nextDynamicToken "{some\ncomment}" (Cursor 0 1) `shouldBe` Just (COMMENT_2 (Cursor 0 1) "{some\ncomment}", "")

    it "STRING_LITERAL str" $
      nextDynamicToken "'str'" (Cursor 0 1) `shouldBe` Just (STRING_LITERAL (Cursor 0 1) "'str'", "")

    it "STRING_LITERAL empty" $
      nextDynamicToken "''" (Cursor 0 1) `shouldBe` Just (STRING_LITERAL (Cursor 0 1) "''", "")

    it "STRING_LITERAL with ' inside'" $
      nextDynamicToken "'abc''cba'" (Cursor 0 1) `shouldBe` Just (STRING_LITERAL (Cursor 0 1) "'abc''cba'", "")

    it "NUM_INT 123" $
      nextDynamicToken "123" (Cursor 0 1) `shouldBe` Just (NUM_INT (Cursor 0 1) "123", "")

    it "NUM_INT 123.34" $
      nextDynamicToken "123.34" (Cursor 0 1) `shouldBe` Just (NUM_INT (Cursor 0 1) "123.34", "")

    it "NUM_INT 123e-34" $
      nextDynamicToken "123e-34" (Cursor 0 1) `shouldBe` Just (NUM_INT (Cursor 0 1) "123e-34", "")

    it "NUM_INT 123.45e56" $
      nextDynamicToken "123.45e56" (Cursor 0 1) `shouldBe` Just (NUM_INT (Cursor 0 1) "123.45e56", "")

    it "IDENT x" $
      nextDynamicToken "x" (Cursor 0 1) `shouldBe` Just (IDENT (Cursor 0 1) "x", "")

    it "IDENT xA9_u" $
      nextDynamicToken "xA9_u" (Cursor 0 1) `shouldBe` Just (IDENT (Cursor 0 1) "xA9_u", "")

    it "IDENT Abc1" $
      nextDynamicToken "Abc1" (Cursor 0 1) `shouldBe` Just (IDENT (Cursor 0 1) "Abc1", "")

  describe "nextToken" $ do
    it "IDEN wins" $
      nextToken "ANDi" (Cursor 0 1) `shouldBe` Just (IDENT (Cursor 0 1) "ANDi", "ANDi", "")

    it "AND wins" $
      nextToken "AND" (Cursor 0 1) `shouldBe` Just (AND (Cursor 0 1), "AND", "")

  describe "tokenize input to list" $ do
    it "empty" $
      tokenize "" `shouldBe` [EOF]

    it "expression" $
      tokenize "if x<y then a:=45" `shouldBe` [
        IF  (Cursor 0 0), WS (Cursor 0 0) " ",
        IDENT (Cursor 0 0) "x", LT_ (Cursor 0 0),IDENT (Cursor 0 0) "y",WS (Cursor 0 0) " ",
        THEN (Cursor 0 0), WS (Cursor 0 0) " ",
        IDENT (Cursor 0 0) "a", ASSIGN (Cursor 0 0), NUM_INT (Cursor 0 0) "45", EOF]

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
