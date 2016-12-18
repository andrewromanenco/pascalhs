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
           | COMMENT_1  -- '(*' .*? '*)' -> skip
           | COMMENT_2  -- '{' .*? '}' -> skip
           | IDENT  -- ('a' .. 'z' | 'A' .. 'Z') ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_')*
           | STRING_LITERAL  -- '\'' ('\'\'' | ~ ('\''))* '\''
           | NUM_INT  -- ('0' .. '9') + (('.' ('0' .. '9') + (EXPONENT)?)? | EXPONENT)
           | EXPONENT -- : ('e') ('+' | '-')? ('0' .. '9') +
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

-- | Produce list of tokens from an input. Fail on error.
tokenize :: [Char] -> [Token]
tokenize (match_token "AND" -> Just (_, restOfInput)) = [AND] ++ tokenize restOfInput
tokenize (match_token "ARRAY" -> Just (_, restOfInput)) = [ARRAY] ++ tokenize restOfInput
tokenize (match_token "BEGIN" -> Just (_, restOfInput)) = [BEGIN] ++ tokenize restOfInput
tokenize (match_token "BOOLEAN" -> Just (_, restOfInput)) = [BOOLEAN] ++ tokenize restOfInput
tokenize (match_token "CASE" -> Just (_, restOfInput)) = [CASE] ++ tokenize restOfInput
tokenize (match_token "CHAR" -> Just (_, restOfInput)) = [CHAR] ++ tokenize restOfInput
tokenize (match_token "CHR" -> Just (_, restOfInput)) = [CHR] ++ tokenize restOfInput
tokenize (match_token "CONST" -> Just (_, restOfInput)) = [CONST] ++ tokenize restOfInput
tokenize (match_token "DIV" -> Just (_, restOfInput)) = [DIV] ++ tokenize restOfInput
tokenize (match_token "DOWNTO" -> Just (_, restOfInput)) = [DOWNTO] ++ tokenize restOfInput
tokenize (match_token "DO" -> Just (_, restOfInput)) = [DO] ++ tokenize restOfInput
tokenize (match_token "ELSE" -> Just (_, restOfInput)) = [ELSE] ++ tokenize restOfInput
tokenize (match_token "END" -> Just (_, restOfInput)) = [END] ++ tokenize restOfInput
tokenize (match_token "FILE" -> Just (_, restOfInput)) = [FILE] ++ tokenize restOfInput
tokenize (match_token "FOR" -> Just (_, restOfInput)) = [FOR] ++ tokenize restOfInput
tokenize (match_token "FUNCTION" -> Just (_, restOfInput)) = [FUNCTION] ++ tokenize restOfInput
tokenize (match_token "GOTO" -> Just (_, restOfInput)) = [GOTO] ++ tokenize restOfInput
tokenize (match_token "IF" -> Just (_, restOfInput)) = [IF] ++ tokenize restOfInput
tokenize (match_token "INTEGER" -> Just (_, restOfInput)) = [INTEGER] ++ tokenize restOfInput
tokenize (match_token "INTERFACE" -> Just (_, restOfInput)) = [INTERFACE] ++ tokenize restOfInput
tokenize (match_token "IN" -> Just (_, restOfInput)) = [IN] ++ tokenize restOfInput
tokenize (match_token "LABEL" -> Just (_, restOfInput)) = [LABEL] ++ tokenize restOfInput
tokenize (match_token "MOD" -> Just (_, restOfInput)) = [MOD] ++ tokenize restOfInput
tokenize (match_token "NIL" -> Just (_, restOfInput)) = [NIL] ++ tokenize restOfInput
tokenize (match_token "NOT" -> Just (_, restOfInput)) = [NOT] ++ tokenize restOfInput
tokenize (match_token "OF" -> Just (_, restOfInput)) = [OF] ++ tokenize restOfInput
tokenize (match_token "OR" -> Just (_, restOfInput)) = [OR] ++ tokenize restOfInput
tokenize (match_token "PACKED" -> Just (_, restOfInput)) = [PACKED] ++ tokenize restOfInput
tokenize (match_token "PROCEDURE" -> Just (_, restOfInput)) = [PROCEDURE] ++ tokenize restOfInput
tokenize (match_token "PROGRAM" -> Just (_, restOfInput)) = [PROGRAM] ++ tokenize restOfInput
tokenize (match_token "REAL" -> Just (_, restOfInput)) = [REAL] ++ tokenize restOfInput
tokenize (match_token "RECORD" -> Just (_, restOfInput)) = [RECORD] ++ tokenize restOfInput
tokenize (match_token "REPEAT" -> Just (_, restOfInput)) = [REPEAT] ++ tokenize restOfInput
tokenize (match_token "SET" -> Just (_, restOfInput)) = [SET] ++ tokenize restOfInput
tokenize (match_token "THEN" -> Just (_, restOfInput)) = [THEN] ++ tokenize restOfInput
tokenize (match_token "TO" -> Just (_, restOfInput)) = [TO] ++ tokenize restOfInput
tokenize (match_token "TYPE" -> Just (_, restOfInput)) = [TYPE] ++ tokenize restOfInput
tokenize (match_token "UNTIL" -> Just (_, restOfInput)) = [UNTIL] ++ tokenize restOfInput
tokenize (match_token "VAR" -> Just (_, restOfInput)) = [VAR] ++ tokenize restOfInput
tokenize (match_token "WHILE" -> Just (_, restOfInput)) = [WHILE] ++ tokenize restOfInput
tokenize (match_token "WITH" -> Just (_, restOfInput)) = [WITH] ++ tokenize restOfInput
tokenize (match_token "\\+" -> Just (_, restOfInput)) = [PLUS] ++ tokenize restOfInput
tokenize (match_token "-" -> Just (_, restOfInput)) = [MINUS] ++ tokenize restOfInput
tokenize (match_token "\\*" -> Just (_, restOfInput)) = [STAR] ++ tokenize restOfInput
tokenize (match_token "/" -> Just (_, restOfInput)) = [SLASH] ++ tokenize restOfInput
tokenize (match_token ":=" -> Just (_, restOfInput)) = [ASSIGN] ++ tokenize restOfInput
tokenize (match_token "," -> Just (_, restOfInput)) = [COMMA] ++ tokenize restOfInput
tokenize (match_token ";" -> Just (_, restOfInput)) = [SEMI] ++ tokenize restOfInput
tokenize (match_token ":" -> Just (_, restOfInput)) = [COLON] ++ tokenize restOfInput
tokenize (match_token "=" -> Just (_, restOfInput)) = [EQUAL] ++ tokenize restOfInput
tokenize (match_token "<>" -> Just (_, restOfInput)) = [NOT_EQUAL] ++ tokenize restOfInput
tokenize (match_token "<=" -> Just (_, restOfInput)) = [LE] ++ tokenize restOfInput
tokenize (match_token "<" -> Just (_, restOfInput)) = [LT_] ++ tokenize restOfInput
tokenize (match_token ">=" -> Just (_, restOfInput)) = [GE] ++ tokenize restOfInput
tokenize (match_token ">" -> Just (_, restOfInput)) = [GT_] ++ tokenize restOfInput
tokenize (match_token "\\(\\." -> Just (_, restOfInput)) = [LBRACK2] ++ tokenize restOfInput
tokenize (match_token "\\(" -> Just (_, restOfInput)) = [LPAREN] ++ tokenize restOfInput
tokenize (match_token "\\)" -> Just (_, restOfInput)) = [RPAREN] ++ tokenize restOfInput
tokenize (match_token "\\[" -> Just (_, restOfInput)) = [LBRACK] ++ tokenize restOfInput
tokenize (match_token "\\]" -> Just (_, restOfInput)) = [RBRACK] ++ tokenize restOfInput
tokenize (match_token "\\.\\)" -> Just (_, restOfInput)) = [RBRACK2] ++ tokenize restOfInput
tokenize (match_token "\\^" -> Just (_, restOfInput)) = [POINTER] ++ tokenize restOfInput
tokenize (match_token "@" -> Just (_, restOfInput)) = [AT] ++ tokenize restOfInput
tokenize (match_token "\\.\\." -> Just (_, restOfInput)) = [DOTDOT] ++ tokenize restOfInput
tokenize (match_token "\\." -> Just (_, restOfInput)) = [DOT] ++ tokenize restOfInput
tokenize (match_token "{" -> Just (_, restOfInput)) = [LCURLY] ++ tokenize restOfInput
tokenize (match_token "}" -> Just (_, restOfInput)) = [RCURLY] ++ tokenize restOfInput
tokenize (match_token "UNIT" -> Just (_, restOfInput)) = [UNIT] ++ tokenize restOfInput
tokenize (match_token "USES" -> Just (_, restOfInput)) = [USES] ++ tokenize restOfInput
tokenize (match_token "STRING" -> Just (_, restOfInput)) = [STRING] ++ tokenize restOfInput
tokenize (match_token "IMPLEMENTATION" -> Just (_, restOfInput)) = [IMPLEMENTATION] ++ tokenize restOfInput
tokenize (match_token "[ \t\r\n]+" -> Just (ws, restOfInput)) = [WS ws] ++ tokenize restOfInput
tokenize [] = [EOF]
