-- | This module takes input string and splits it into list of tokens.

{-# LANGUAGE ViewPatterns #-}

module Lexer where

import Data.List
import Data.Maybe
import Text.Regex.PCRE


-- | Valid tokens for Pascal lang.
-- As defined in res/pascal.g4
data Token = EOF  -- End of file/input
           | AND  -- A N D
           | ARRAY  -- A R R A Y
           | BEGIN  -- B E G I N
           | BOOLEAN  -- B O O L E A N
           | CASE  -- C A S E
           | CHAR  -- C H A R
           | CHR  -- C H R
           | CONST  -- C O N S T
           | DIV  -- D I V
           | DO  -- D O
           | DOWNTO  -- D O W N T O
           | ELSE  -- E L S E
           | END  -- E N D
           | FILE  -- F I L E
           | FOR  -- F O R
           | FUNCTION  -- F U N C T I O N
           | GOTO  -- G O T O
           | IF  -- I F
           | IN  -- I N
           | INTEGER  -- I N T E G E R
           | LABEL  -- L A B E L
           | MOD  -- M O D
           | NIL  -- N I L
           | NOT  -- N O T
           | OF  -- O F
           | OR  -- O R
           | PACKED  -- P A C K E D
           | PROCEDURE  -- P R O C E D U R E
           | PROGRAM  -- P R O G R A M
           | REAL  -- R E A L
           | RECORD  -- R E C O R D
           | REPEAT  -- R E P E A T
           | SET  -- S E T
           | THEN  -- T H E N
           | TO  -- T O
           | TYPE  -- T Y P E
           | UNTIL  -- U N T I L
           | VAR  -- V A R
           | WHILE  -- W H I L E
           | WITH  -- W I T H
           | PLUS  -- '+'
           | MINUS  -- '-'
           | STAR  -- '*'
           | SLASH  -- '/'
           | ASSIGN  -- ':='
           | COMMA  -- ','
           | SEMI  -- ';'
           | COLON  -- ':'
           | EQUAL  -- '='
           | NOT_EQUAL  -- '<>'
           | LT_  -- '<'
           | LE  -- '<='
           | GE  -- '>='
           | GT_  -- '>'
           | LPAREN  -- '('
           | RPAREN  -- ')'
           | LBRACK  -- '['
           | LBRACK2  -- '(.'
           | RBRACK  -- ']'
           | RBRACK2  -- '.)'
           | POINTER  -- '^'
           | AT  -- '@'
           | DOT  -- '.'
           | DOTDOT  -- '..'
           | LCURLY  -- '{'
           | RCURLY  -- '}'
           | UNIT  -- U N I T
           | INTERFACE  -- I N T E R F A C E
           | USES  -- U S E S
           | STRING  -- S T R I N G
           | IMPLEMENTATION  -- I M P L E M E N T A T I O N
           | WS String  -- [ \t\r\n] -> skip
           | COMMENT_1 String  -- '(*' .*? '*)' -> skip
           | COMMENT_2 String  -- '{' .*? '}' -> skip
           | IDENT String  -- ('a' .. 'z' | 'A' .. 'Z') ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_')*
           | STRING_LITERAL String  -- '\'' ('\'\'' | ~ ('\''))* '\''
           | NUM_INT String  -- ('0' .. '9') + (('.' ('0' .. '9') + (EXPONENT)?)? | EXPONENT)
           deriving (Show, Eq)


-- | Cuts off existing prefix from a string. Attempt to cut off non existing
-- prefix will fail at runtime.
stripExistingPrefix :: [Char] -> [Char] -> [Char]
stripExistingPrefix  prefix str = fromJust (stripPrefix prefix str)


-- | Case insensitive regex: http://stackoverflow.com/a/1008334
(=~+) ::
   ( RegexMaker regex compOpt execOpt source
   , RegexContext regex source1 target )
   => source1 -> (source, compOpt, execOpt) -> target
source1 =~+ (source, compOpt, execOpt)
  = match (makeRegexOpts compOpt execOpt source) source1


-- | Match POSIX regex to the beginning of given list. Case insensitive.
-- Result is token matched and the rest of the list; or nothing.
match_token :: [Char] -> [Char] -> Maybe([Char], [Char])
match_token pattern str = let matched = (str =~+ ("^" ++ pattern, compCaseless + compUTF8, execBlank) :: String)
 in if matched == []
   then Nothing
   else Just (matched, stripExistingPrefix matched str)

-- | Produce token, match and rest of the input. Or nothing if no token found.
-- e.g. "and not" -> (AND, "and", " not")
nextStaticToken :: [Char] -> Maybe (Token, [Char], [Char])
nextStaticToken (match_token "AND" -> Just (matched, restOfInput)) = Just (AND, matched, restOfInput)
nextStaticToken (match_token "ARRAY" -> Just (matched, restOfInput)) = Just (ARRAY, matched, restOfInput)
nextStaticToken (match_token "BEGIN" -> Just (matched, restOfInput)) = Just (BEGIN, matched, restOfInput)
nextStaticToken (match_token "BOOLEAN" -> Just (matched, restOfInput)) = Just (BOOLEAN, matched, restOfInput)
nextStaticToken (match_token "CASE" -> Just (matched, restOfInput)) = Just (CASE, matched, restOfInput)
nextStaticToken (match_token "CHAR" -> Just (matched, restOfInput)) = Just (CHAR, matched, restOfInput)
nextStaticToken (match_token "CHR" -> Just (matched, restOfInput)) = Just (CHR, matched, restOfInput)
nextStaticToken (match_token "CONST" -> Just (matched, restOfInput)) = Just (CONST, matched, restOfInput)
nextStaticToken (match_token "DIV" -> Just (matched, restOfInput)) = Just (DIV, matched, restOfInput)
nextStaticToken (match_token "DOWNTO" -> Just (matched, restOfInput)) = Just (DOWNTO, matched, restOfInput)
nextStaticToken (match_token "DO" -> Just (matched, restOfInput)) = Just (DO, matched, restOfInput)
nextStaticToken (match_token "ELSE" -> Just (matched, restOfInput)) = Just (ELSE, matched, restOfInput)
nextStaticToken (match_token "END" -> Just (matched, restOfInput)) = Just (END, matched, restOfInput)
nextStaticToken (match_token "FILE" -> Just (matched, restOfInput)) = Just (FILE, matched, restOfInput)
nextStaticToken (match_token "FOR" -> Just (matched, restOfInput)) = Just (FOR, matched, restOfInput)
nextStaticToken (match_token "FUNCTION" -> Just (matched, restOfInput)) = Just (FUNCTION, matched, restOfInput)
nextStaticToken (match_token "GOTO" -> Just (matched, restOfInput)) = Just (GOTO, matched, restOfInput)
nextStaticToken (match_token "IF" -> Just (matched, restOfInput)) = Just (IF, matched, restOfInput)
nextStaticToken (match_token "INTEGER" -> Just (matched, restOfInput)) = Just (INTEGER, matched, restOfInput)
nextStaticToken (match_token "INTERFACE" -> Just (matched, restOfInput)) = Just (INTERFACE, matched, restOfInput)
nextStaticToken (match_token "IN" -> Just (matched, restOfInput)) = Just (IN, matched, restOfInput)
nextStaticToken (match_token "LABEL" -> Just (matched, restOfInput)) = Just (LABEL, matched, restOfInput)
nextStaticToken (match_token "MOD" -> Just (matched, restOfInput)) = Just (MOD, matched, restOfInput)
nextStaticToken (match_token "NIL" -> Just (matched, restOfInput)) = Just (NIL, matched, restOfInput)
nextStaticToken (match_token "NOT" -> Just (matched, restOfInput)) = Just (NOT, matched, restOfInput)
nextStaticToken (match_token "OF" -> Just (matched, restOfInput)) = Just (OF, matched, restOfInput)
nextStaticToken (match_token "OR" -> Just (matched, restOfInput)) = Just (OR, matched, restOfInput)
nextStaticToken (match_token "PACKED" -> Just (matched, restOfInput)) = Just (PACKED, matched, restOfInput)
nextStaticToken (match_token "PROCEDURE" -> Just (matched, restOfInput)) = Just (PROCEDURE, matched, restOfInput)
nextStaticToken (match_token "PROGRAM" -> Just (matched, restOfInput)) = Just (PROGRAM, matched, restOfInput)
nextStaticToken (match_token "REAL" -> Just (matched, restOfInput)) = Just (REAL, matched, restOfInput)
nextStaticToken (match_token "RECORD" -> Just (matched, restOfInput)) = Just (RECORD, matched, restOfInput)
nextStaticToken (match_token "REPEAT" -> Just (matched, restOfInput)) = Just (REPEAT, matched, restOfInput)
nextStaticToken (match_token "SET" -> Just (matched, restOfInput)) = Just (SET, matched, restOfInput)
nextStaticToken (match_token "THEN" -> Just (matched, restOfInput)) = Just (THEN, matched, restOfInput)
nextStaticToken (match_token "TO" -> Just (matched, restOfInput)) = Just (TO, matched, restOfInput)
nextStaticToken (match_token "TYPE" -> Just (matched, restOfInput)) = Just (TYPE, matched, restOfInput)
nextStaticToken (match_token "UNTIL" -> Just (matched, restOfInput)) = Just (UNTIL, matched, restOfInput)
nextStaticToken (match_token "VAR" -> Just (matched, restOfInput)) = Just (VAR, matched, restOfInput)
nextStaticToken (match_token "WHILE" -> Just (matched, restOfInput)) = Just (WHILE, matched, restOfInput)
nextStaticToken (match_token "WITH" -> Just (matched, restOfInput)) = Just (WITH, matched, restOfInput)
nextStaticToken (match_token "\\+" -> Just (matched, restOfInput)) = Just (PLUS, matched, restOfInput)
nextStaticToken (match_token "-" -> Just (matched, restOfInput)) = Just (MINUS, matched, restOfInput)
nextStaticToken (match_token "\\*" -> Just (matched, restOfInput)) = Just (STAR, matched, restOfInput)
nextStaticToken (match_token "/" -> Just (matched, restOfInput)) = Just (SLASH, matched, restOfInput)
nextStaticToken (match_token ":=" -> Just (matched, restOfInput)) = Just (ASSIGN, matched, restOfInput)
nextStaticToken (match_token "," -> Just (matched, restOfInput)) = Just (COMMA, matched, restOfInput)
nextStaticToken (match_token ";" -> Just (matched, restOfInput)) = Just (SEMI, matched, restOfInput)
nextStaticToken (match_token ":" -> Just (matched, restOfInput)) = Just (COLON, matched, restOfInput)
nextStaticToken (match_token "=" -> Just (matched, restOfInput)) = Just (EQUAL, matched, restOfInput)
nextStaticToken (match_token "<>" -> Just (matched, restOfInput)) = Just (NOT_EQUAL, matched, restOfInput)
nextStaticToken (match_token "<=" -> Just (matched, restOfInput)) = Just (LE, matched, restOfInput)
nextStaticToken (match_token "<" -> Just (matched, restOfInput)) = Just (LT_, matched, restOfInput)
nextStaticToken (match_token ">=" -> Just (matched, restOfInput)) = Just (GE, matched, restOfInput)
nextStaticToken (match_token ">" -> Just (matched, restOfInput)) = Just (GT_, matched, restOfInput)
nextStaticToken (match_token "\\(\\." -> Just (matched, restOfInput)) = Just (LBRACK2, matched, restOfInput)
nextStaticToken (match_token "\\(" -> Just (matched, restOfInput)) = Just (LPAREN, matched, restOfInput)
nextStaticToken (match_token "\\)" -> Just (matched, restOfInput)) = Just (RPAREN, matched, restOfInput)
nextStaticToken (match_token "\\[" -> Just (matched, restOfInput)) = Just (LBRACK, matched, restOfInput)
nextStaticToken (match_token "\\]" -> Just (matched, restOfInput)) = Just (RBRACK, matched, restOfInput)
nextStaticToken (match_token "\\.\\)" -> Just (matched, restOfInput)) = Just (RBRACK2, matched, restOfInput)
nextStaticToken (match_token "\\^" -> Just (matched, restOfInput)) = Just (POINTER, matched, restOfInput)
nextStaticToken (match_token "@" -> Just (matched, restOfInput)) = Just (AT, matched, restOfInput)
nextStaticToken (match_token "\\.\\." -> Just (matched, restOfInput)) = Just (DOTDOT, matched, restOfInput)
nextStaticToken (match_token "\\." -> Just (matched, restOfInput)) = Just (DOT, matched, restOfInput)
nextStaticToken (match_token "{" -> Just (matched, restOfInput)) = Just (LCURLY, matched, restOfInput)
nextStaticToken (match_token "}" -> Just (matched, restOfInput)) = Just (RCURLY, matched, restOfInput)
nextStaticToken (match_token "UNIT" -> Just (matched, restOfInput)) = Just (UNIT, matched, restOfInput)
nextStaticToken (match_token "USES" -> Just (matched, restOfInput)) = Just (USES, matched, restOfInput)
nextStaticToken (match_token "STRING" -> Just (matched, restOfInput)) = Just (STRING, matched, restOfInput)
nextStaticToken (match_token "IMPLEMENTATION" -> Just (matched, restOfInput)) = Just (IMPLEMENTATION, matched, restOfInput)
nextStaticToken _ = Nothing
