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

  describe "move position" $ do
    it "no move on empty" $
      moveCursor (Pos 1 2) "" `shouldBe` Pos 1 2

    it "move left" $
      moveCursor (Pos 1 2) "abc" `shouldBe` Pos 1 5

    it "move left ignoring \\r" $
      moveCursor (Pos 1 2) "a\rt" `shouldBe` Pos 1 4

    it "move left and down" $
      moveCursor (Pos 1 2) "abc\n" `shouldBe` Pos 2 1

    it "move left down left" $
      moveCursor (Pos 1 2) "abc\nabc" `shouldBe` Pos 2 4

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
      nextStaticToken "and" (Pos 0 1) `shouldBe` Just (AND (Pos 0 1), "and", "")

    it "ARRAY" $
      nextStaticToken "array" (Pos 0 1) `shouldBe` Just (ARRAY (Pos 0 1), "array", "")

    it "BEGIN" $
      nextStaticToken "begin" (Pos 0 1) `shouldBe` Just (BEGIN (Pos 0 1), "begin", "")

    it "BOOLEAN" $
      nextStaticToken "boolean" (Pos 0 1) `shouldBe` Just (BOOLEAN (Pos 0 1), "boolean", "")

    it "CASE" $
      nextStaticToken "case" (Pos 0 1) `shouldBe` Just (CASE (Pos 0 1), "case", "")

    it "CHAR" $
      nextStaticToken "char" (Pos 0 1) `shouldBe` Just (CHAR (Pos 0 1), "char", "")

    it "CHR" $
      nextStaticToken "chr" (Pos 0 1) `shouldBe` Just (CHR (Pos 0 1), "chr", "")

    it "CONST" $
      nextStaticToken "const" (Pos 0 1) `shouldBe` Just (CONST (Pos 0 1), "const", "")

    it "DIV" $
      nextStaticToken "div" (Pos 0 1) `shouldBe` Just (DIV (Pos 0 1), "div", "")

    it "DO" $
      nextStaticToken "do" (Pos 0 1) `shouldBe` Just (DO (Pos 0 1), "do", "")

    it "DOWNTO" $
      nextStaticToken "downto" (Pos 0 1) `shouldBe` Just (DOWNTO (Pos 0 1), "downto", "")

    it "ELSE" $
      nextStaticToken "else" (Pos 0 1) `shouldBe` Just (ELSE (Pos 0 1), "else", "")

    it "END" $
      nextStaticToken "end" (Pos 0 1) `shouldBe` Just (END (Pos 0 1), "end", "")

    it "FILE" $
      nextStaticToken "file" (Pos 0 1) `shouldBe` Just (FILE (Pos 0 1), "file", "")

    it "FOR" $
      nextStaticToken "for" (Pos 0 1) `shouldBe` Just (FOR (Pos 0 1), "for", "")

    it "FUNCTION" $
      nextStaticToken "function" (Pos 0 1) `shouldBe` Just (FUNCTION (Pos 0 1), "function", "")

    it "GOTO" $
      nextStaticToken "goto" (Pos 0 1) `shouldBe` Just (GOTO (Pos 0 1), "goto", "")

    it "IF" $
      nextStaticToken "if" (Pos 0 1) `shouldBe` Just (IF (Pos 0 1), "if", "")

    it "IN" $
      nextStaticToken "in" (Pos 0 1) `shouldBe` Just (IN (Pos 0 1), "in", "")

    it "INTEGER" $
      nextStaticToken "integer" (Pos 0 1) `shouldBe` Just (INTEGER (Pos 0 1), "integer", "")

    it "LABEL" $
      nextStaticToken "label" (Pos 0 1) `shouldBe` Just (LABEL (Pos 0 1), "label", "")

    it "MOD" $
      nextStaticToken "mod" (Pos 0 1) `shouldBe` Just (MOD (Pos 0 1), "mod", "")

    it "NIL" $
      nextStaticToken "nil" (Pos 0 1) `shouldBe` Just (NIL (Pos 0 1), "nil", "")

    it "NOT" $
      nextStaticToken "not" (Pos 0 1) `shouldBe` Just (NOT (Pos 0 1), "not", "")

    it "OF" $
      nextStaticToken "of" (Pos 0 1) `shouldBe` Just (OF (Pos 0 1), "of", "")

    it "OR" $
      nextStaticToken "or" (Pos 0 1) `shouldBe` Just (OR (Pos 0 1), "or", "")

    it "PACKED" $
      nextStaticToken "packed" (Pos 0 1) `shouldBe` Just (PACKED (Pos 0 1), "packed", "")

    it "PROCEDURE" $
      nextStaticToken "procedure" (Pos 0 1) `shouldBe` Just (PROCEDURE (Pos 0 1), "procedure", "")

    it "PROGRAM" $
      nextStaticToken "program" (Pos 0 1) `shouldBe` Just (PROGRAM (Pos 0 1), "program", "")

    it "REAL" $
      nextStaticToken "real" (Pos 0 1) `shouldBe` Just (REAL (Pos 0 1), "real", "")

    it "RECORD" $
      nextStaticToken "record" (Pos 0 1) `shouldBe` Just (RECORD (Pos 0 1), "record", "")

    it "REPEAT" $
      nextStaticToken "repeat" (Pos 0 1) `shouldBe` Just (REPEAT (Pos 0 1), "repeat", "")

    it "SET" $
      nextStaticToken "set" (Pos 0 1) `shouldBe` Just (SET (Pos 0 1), "set", "")

    it "THEN" $
      nextStaticToken "then" (Pos 0 1) `shouldBe` Just (THEN (Pos 0 1), "then", "")

    it "TO" $
      nextStaticToken "to" (Pos 0 1) `shouldBe` Just (TO (Pos 0 1), "to", "")

    it "TYPE" $
      nextStaticToken "type" (Pos 0 1) `shouldBe` Just (TYPE (Pos 0 1), "type", "")

    it "UNTIL" $
      nextStaticToken "until" (Pos 0 1) `shouldBe` Just (UNTIL (Pos 0 1), "until", "")

    it "VAR" $
      nextStaticToken "var" (Pos 0 1) `shouldBe` Just (VAR (Pos 0 1), "var", "")

    it "WHILE" $
      nextStaticToken "while" (Pos 0 1) `shouldBe` Just (WHILE (Pos 0 1), "while", "")

    it "WITH" $
      nextStaticToken "with" (Pos 0 1) `shouldBe` Just (WITH (Pos 0 1), "with", "")

    it "PLUS" $
      nextStaticToken "+" (Pos 0 1) `shouldBe` Just (PLUS (Pos 0 1), "+", "")

    it "MINUS" $
      nextStaticToken "-" (Pos 0 1) `shouldBe` Just (MINUS (Pos 0 1), "-", "")

    it "STAR" $
      nextStaticToken "*" (Pos 0 1) `shouldBe` Just (STAR (Pos 0 1), "*", "")

    it "SLASH" $
      nextStaticToken "/" (Pos 0 1) `shouldBe` Just (SLASH (Pos 0 1), "/", "")

    it "ASSIGN" $
      nextStaticToken ":=" (Pos 0 1) `shouldBe` Just (ASSIGN (Pos 0 1), ":=", "")

    it "COMMA" $
      nextStaticToken "," (Pos 0 1) `shouldBe` Just (COMMA (Pos 0 1), ",", "")

    it "SEMI" $
      nextStaticToken ";" (Pos 0 1) `shouldBe` Just (SEMI (Pos 0 1), ";", "")

    it "COLON" $
      nextStaticToken ":" (Pos 0 1) `shouldBe` Just (COLON (Pos 0 1), ":", "")

    it "EQUAL" $
      nextStaticToken "=" (Pos 0 1) `shouldBe` Just (EQUAL (Pos 0 1), "=", "")

    it "NOT_EQUAL" $
      nextStaticToken "<>" (Pos 0 1) `shouldBe` Just (NOT_EQUAL (Pos 0 1), "<>", "")

    it "LT" $
      nextStaticToken "<" (Pos 0 1) `shouldBe` Just (LT_ (Pos 0 1), "<", "")

    it "LE" $
      nextStaticToken "<=" (Pos 0 1) `shouldBe` Just (LE (Pos 0 1), "<=", "")

    it "GE" $
      nextStaticToken ">=" (Pos 0 1) `shouldBe` Just (GE (Pos 0 1), ">=", "")

    it "GT" $
      nextStaticToken ">" (Pos 0 1) `shouldBe` Just (GT_ (Pos 0 1), ">", "")

    it "LPAREN" $
      nextStaticToken "(" (Pos 0 1) `shouldBe` Just (LPAREN (Pos 0 1), "(", "")

    it "RPAREN" $
      nextStaticToken ")" (Pos 0 1) `shouldBe` Just (RPAREN (Pos 0 1), ")", "")

    it "LBRACK" $
      nextStaticToken "[" (Pos 0 1) `shouldBe` Just (LBRACK (Pos 0 1), "[", "")

    it "LBRACK2" $
      nextStaticToken "(." (Pos 0 1) `shouldBe` Just (LBRACK2 (Pos 0 1), "(.", "")

    it "RBRACK" $
      nextStaticToken "]" (Pos 0 1) `shouldBe` Just (RBRACK (Pos 0 1), "]", "")

    it "RBRACK2" $
      nextStaticToken ".)" (Pos 0 1) `shouldBe` Just (RBRACK2 (Pos 0 1), ".)", "")

    it "POINTER" $
      nextStaticToken "^" (Pos 0 1) `shouldBe` Just (POINTER (Pos 0 1), "^", "")

    it "AT" $
      nextStaticToken "@" (Pos 0 1) `shouldBe` Just (AT (Pos 0 1), "@", "")

    it "DOT" $
      nextStaticToken "." (Pos 0 1) `shouldBe` Just (DOT (Pos 0 1), ".", "")

    it "DOTDOT" $
      nextStaticToken ".." (Pos 0 1) `shouldBe` Just (DOTDOT (Pos 0 1), "..", "")

    it "LCURLY" $
      nextStaticToken "{" (Pos 0 1) `shouldBe` Just (LCURLY (Pos 0 1), "{", "")

    it "RCURLY" $
      nextStaticToken "}" (Pos 0 1) `shouldBe` Just (RCURLY (Pos 0 1), "}", "")

    it "UNIT" $
      nextStaticToken "unit" (Pos 0 1) `shouldBe` Just (UNIT (Pos 0 1), "unit", "")

    it "INTERFACE" $
      nextStaticToken "interface" (Pos 0 1) `shouldBe` Just (INTERFACE (Pos 0 1), "interface", "")

    it "USES" $
      nextStaticToken "uses" (Pos 0 1) `shouldBe` Just (USES (Pos 0 1), "uses", "")

    it "STRING" $
      nextStaticToken "string" (Pos 0 1) `shouldBe` Just (STRING (Pos 0 1), "string", "")

    it "IMPLEMENTATION" $
      nextStaticToken "implementation" (Pos 0 1) `shouldBe` Just (IMPLEMENTATION (Pos 0 1), "implementation", "")

  describe "tokenize dynamic" $ do
    it "WS" $
      nextDynamicToken "  \t  \r\n " (Pos 0 1) `shouldBe` Just (WS (Pos 0 1) "  \t  \r\n ", "")

    it "Comment1 single line" $
      nextDynamicToken "(*some comment*)" (Pos 0 1) `shouldBe` Just (COMMENT_1 (Pos 0 1) "(*some comment*)", "")

    it "Comment1 multiple lines" $
      nextDynamicToken "(*some\ncomment*)" (Pos 0 1) `shouldBe` Just (COMMENT_1 (Pos 0 1) "(*some\ncomment*)", "")

    it "Comment2 single line" $
      nextDynamicToken "{some comment}" (Pos 0 1) `shouldBe` Just (COMMENT_2 (Pos 0 1) "{some comment}", "")

    it "Comment2 multiple lines" $
      nextDynamicToken "{some\ncomment}" (Pos 0 1) `shouldBe` Just (COMMENT_2 (Pos 0 1) "{some\ncomment}", "")

    it "STRING_LITERAL str" $
      nextDynamicToken "'str'" (Pos 0 1) `shouldBe` Just (STRING_LITERAL (Pos 0 1) "'str'", "")

    it "STRING_LITERAL empty" $
      nextDynamicToken "''" (Pos 0 1) `shouldBe` Just (STRING_LITERAL (Pos 0 1) "''", "")

    it "STRING_LITERAL with ' inside'" $
      nextDynamicToken "'abc''cba'" (Pos 0 1) `shouldBe` Just (STRING_LITERAL (Pos 0 1) "'abc''cba'", "")

    it "NUM_INT 123" $
      nextDynamicToken "123" (Pos 0 1) `shouldBe` Just (NUM_INT (Pos 0 1) "123", "")

    it "NUM_INT 123.34" $
      nextDynamicToken "123.34" (Pos 0 1) `shouldBe` Just (NUM_INT (Pos 0 1) "123.34", "")

    it "NUM_INT 123e-34" $
      nextDynamicToken "123e-34" (Pos 0 1) `shouldBe` Just (NUM_INT (Pos 0 1) "123e-34", "")

    it "NUM_INT 123.45e56" $
      nextDynamicToken "123.45e56" (Pos 0 1) `shouldBe` Just (NUM_INT (Pos 0 1) "123.45e56", "")

    it "IDENT x" $
      nextDynamicToken "x" (Pos 0 1) `shouldBe` Just (IDENT (Pos 0 1) "x", "")

    it "IDENT xA9_u" $
      nextDynamicToken "xA9_u" (Pos 0 1) `shouldBe` Just (IDENT (Pos 0 1) "xA9_u", "")

    it "IDENT Abc1" $
      nextDynamicToken "Abc1" (Pos 0 1) `shouldBe` Just (IDENT (Pos 0 1) "Abc1", "")

  describe "nextToken" $ do
    it "IDEN wins" $
      nextToken "ANDi" (Pos 0 1) `shouldBe` Just (IDENT (Pos 0 1) "ANDi", "ANDi", "")

    it "AND wins" $
      nextToken "AND" (Pos 0 1) `shouldBe` Just (AND (Pos 0 1), "AND", "")

  describe "tokenize input to list" $ do
    it "empty" $
      tokenize "" `shouldBe` [EOF]

    it "expression" $
      tokenize "if x<y then a:=45" `shouldBe` [
        IF  (Pos 0 0), WS (Pos 0 0) " ",
        IDENT (Pos 0 0) "x", LT_ (Pos 0 0),IDENT (Pos 0 0) "y",WS (Pos 0 0) " ",
        THEN (Pos 0 0), WS (Pos 0 0) " ",
        IDENT (Pos 0 0) "a", ASSIGN (Pos 0 0), NUM_INT (Pos 0 0) "45", EOF]

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
