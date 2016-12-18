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
           | LT  -- '<'
           | LE  -- '<='
           | GE  -- '>='
           | GT  -- '>'
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
tokenize (match_token "and" -> Just (_, restOfInput)) = [AND] ++ tokenize restOfInput
tokenize (match_token "array" -> Just (_, restOfInput)) = [ARRAY] ++ tokenize restOfInput
tokenize (match_token "[ \t\r\n]+" -> Just (ws, restOfInput)) = [WS ws] ++ tokenize restOfInput
tokenize [] = [EOF]
