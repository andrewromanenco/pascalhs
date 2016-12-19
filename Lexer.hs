-- | This module takes input string and splits it into list of tokens.

{-# LANGUAGE ViewPatterns #-}

module Lexer where

import Data.List
import Data.Maybe
import Text.Regex.PCRE


-- | Position in input text; line and column.
data Cursor = Cursor Int Int
  deriving(Eq)


-- | Show for Cursor.
instance Show Cursor where
  show (Cursor a b) = "{Ln: " ++ (show a) ++ ", Col: " ++ (show b) ++ "}"


-- | Get line number from a position.
lineNumber :: Cursor -> Int
lineNumber (Cursor n _) = n


-- | Get Column number from a position.
column :: Cursor -> Int
column (Cursor _ n) = n


-- | Moves cursor forward to new position.
moveCursor :: Cursor -> [Char] -> Cursor
moveCursor (Cursor l c) []  = Cursor l c
moveCursor p@(Cursor l c) ('\r':xs)  = moveCursor p xs
moveCursor (Cursor l c) ('\n':xs)  = moveCursor (Cursor (l + 1) 1) xs
moveCursor (Cursor l c) (_:xs)  = moveCursor (Cursor l (c + 1)) xs


-- | Valid tokens for Pascal lang.
-- As defined in res/pascal.g4
data Token = EOF  -- End of file/input
           | AND Cursor  -- A N D
           | ARRAY Cursor  -- A R R A Y
           | BEGIN Cursor  -- B E G I N
           | BOOLEAN Cursor  -- B O O L E A N
           | CASE Cursor  -- C A S E
           | CHAR Cursor  -- C H A R
           | CHR Cursor  -- C H R
           | CONST Cursor  -- C O N S T
           | DIV Cursor  -- D I V
           | DO Cursor  -- D O
           | DOWNTO Cursor  -- D O W N T O
           | ELSE Cursor  -- E L S E
           | END Cursor  -- E N D
           | FILE Cursor  -- F I L E
           | FOR Cursor  -- F O R
           | FUNCTION Cursor  -- F U N C T I O N
           | GOTO Cursor  -- G O T O
           | IF Cursor  -- I F
           | IN Cursor  -- I N
           | INTEGER Cursor  -- I N T E G E R
           | LABEL Cursor  -- L A B E L
           | MOD Cursor  -- M O D
           | NIL Cursor  -- N I L
           | NOT Cursor  -- N O T
           | OF Cursor  -- O F
           | OR Cursor  -- O R
           | PACKED Cursor  -- P A C K E D
           | PROCEDURE Cursor  -- P R O C E D U R E
           | PROGRAM Cursor  -- P R O G R A M
           | REAL Cursor  -- R E A L
           | RECORD Cursor  -- R E C O R D
           | REPEAT Cursor  -- R E P E A T
           | SET Cursor  -- S E T
           | THEN Cursor  -- T H E N
           | TO Cursor  -- T O
           | TYPE Cursor  -- T Y P E
           | UNTIL Cursor  -- U N T I L
           | VAR Cursor  -- V A R
           | WHILE Cursor  -- W H I L E
           | WITH Cursor  -- W I T H
           | PLUS Cursor  -- '+'
           | MINUS Cursor  -- '-'
           | STAR Cursor  -- '*'
           | SLASH Cursor  -- '/'
           | ASSIGN Cursor  -- ':='
           | COMMA Cursor  -- ','
           | SEMI Cursor  -- ';'
           | COLON Cursor  -- ':'
           | EQUAL Cursor  -- '='
           | NOT_EQUAL Cursor  -- '<>'
           | LT_ Cursor  -- '<'
           | LE Cursor  -- '<='
           | GE Cursor  -- '>='
           | GT_ Cursor  -- '>'
           | LPAREN Cursor  -- '('
           | RPAREN Cursor  -- ')'
           | LBRACK Cursor  -- '['
           | LBRACK2 Cursor  -- '(.'
           | RBRACK Cursor  -- ']'
           | RBRACK2 Cursor  -- '.)'
           | POINTER Cursor  -- '^'
           | AT Cursor  -- '@'
           | DOT Cursor  -- '.'
           | DOTDOT Cursor  -- '..'
           | LCURLY Cursor  -- '{'
           | RCURLY Cursor  -- '}'
           | UNIT Cursor  -- U N I T
           | INTERFACE Cursor  -- I N T E R F A C E
           | USES Cursor  -- U S E S
           | STRING Cursor  -- S T R I N G
           | IMPLEMENTATION Cursor  -- I M P L E M E N T A T I O N
           | WS Cursor String  -- [ \t\r\n] -> skip
           | COMMENT_1 Cursor String  -- '(*' .*? '*)' -> skip
           | COMMENT_2 Cursor String  -- '{' .*? '}' -> skip
           | IDENT Cursor String  -- ('a' .. 'z' | 'A' .. 'Z') ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_')*
           | STRING_LITERAL Cursor String  -- '\'' ('\'\'' | ~ ('\''))* '\''
           | NUM_INT Cursor String  -- ('0' .. '9') + (('.' ('0' .. '9') + (EXPONENT)?)? | EXPONENT)
           deriving (Show, Eq)


-- | Extract value from a dynamic token.
tokenValue :: Token -> String
tokenValue(WS _ s) = s
tokenValue(COMMENT_1 _ s) = s
tokenValue(COMMENT_2 _ s) = s
tokenValue(IDENT _ s) = s
tokenValue(STRING_LITERAL _ s) = s
tokenValue(NUM_INT _ s) = s


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
nextStaticToken :: [Char] -> Cursor -> Maybe (Token, [Char], [Char])
nextStaticToken (match_token "AND" -> Just (matched, restOfInput)) pos = Just (AND pos, matched, restOfInput)
nextStaticToken (match_token "ARRAY" -> Just (matched, restOfInput)) pos = Just (ARRAY pos, matched, restOfInput)
nextStaticToken (match_token "BEGIN" -> Just (matched, restOfInput)) pos = Just (BEGIN pos, matched, restOfInput)
nextStaticToken (match_token "BOOLEAN" -> Just (matched, restOfInput)) pos = Just (BOOLEAN pos, matched, restOfInput)
nextStaticToken (match_token "CASE" -> Just (matched, restOfInput)) pos = Just (CASE pos, matched, restOfInput)
nextStaticToken (match_token "CHAR" -> Just (matched, restOfInput)) pos = Just (CHAR pos, matched, restOfInput)
nextStaticToken (match_token "CHR" -> Just (matched, restOfInput)) pos = Just (CHR pos, matched, restOfInput)
nextStaticToken (match_token "CONST" -> Just (matched, restOfInput)) pos = Just (CONST pos, matched, restOfInput)
nextStaticToken (match_token "DIV" -> Just (matched, restOfInput)) pos = Just (DIV pos, matched, restOfInput)
nextStaticToken (match_token "DOWNTO" -> Just (matched, restOfInput)) pos = Just (DOWNTO pos, matched, restOfInput)
nextStaticToken (match_token "DO" -> Just (matched, restOfInput)) pos = Just (DO pos, matched, restOfInput)
nextStaticToken (match_token "ELSE" -> Just (matched, restOfInput)) pos = Just (ELSE pos, matched, restOfInput)
nextStaticToken (match_token "END" -> Just (matched, restOfInput)) pos = Just (END pos, matched, restOfInput)
nextStaticToken (match_token "FILE" -> Just (matched, restOfInput)) pos = Just (FILE pos, matched, restOfInput)
nextStaticToken (match_token "FOR" -> Just (matched, restOfInput)) pos = Just (FOR pos, matched, restOfInput)
nextStaticToken (match_token "FUNCTION" -> Just (matched, restOfInput)) pos = Just (FUNCTION pos, matched, restOfInput)
nextStaticToken (match_token "GOTO" -> Just (matched, restOfInput)) pos = Just (GOTO pos, matched, restOfInput)
nextStaticToken (match_token "IF" -> Just (matched, restOfInput)) pos = Just (IF pos, matched, restOfInput)
nextStaticToken (match_token "INTEGER" -> Just (matched, restOfInput)) pos = Just (INTEGER pos, matched, restOfInput)
nextStaticToken (match_token "INTERFACE" -> Just (matched, restOfInput)) pos = Just (INTERFACE pos, matched, restOfInput)
nextStaticToken (match_token "IN" -> Just (matched, restOfInput)) pos = Just (IN pos, matched, restOfInput)
nextStaticToken (match_token "LABEL" -> Just (matched, restOfInput)) pos = Just (LABEL pos, matched, restOfInput)
nextStaticToken (match_token "MOD" -> Just (matched, restOfInput)) pos = Just (MOD pos, matched, restOfInput)
nextStaticToken (match_token "NIL" -> Just (matched, restOfInput)) pos = Just (NIL pos, matched, restOfInput)
nextStaticToken (match_token "NOT" -> Just (matched, restOfInput)) pos = Just (NOT pos, matched, restOfInput)
nextStaticToken (match_token "OF" -> Just (matched, restOfInput)) pos = Just (OF pos, matched, restOfInput)
nextStaticToken (match_token "OR" -> Just (matched, restOfInput)) pos = Just (OR pos, matched, restOfInput)
nextStaticToken (match_token "PACKED" -> Just (matched, restOfInput)) pos = Just (PACKED pos, matched, restOfInput)
nextStaticToken (match_token "PROCEDURE" -> Just (matched, restOfInput)) pos = Just (PROCEDURE pos, matched, restOfInput)
nextStaticToken (match_token "PROGRAM" -> Just (matched, restOfInput)) pos = Just (PROGRAM pos, matched, restOfInput)
nextStaticToken (match_token "REAL" -> Just (matched, restOfInput)) pos = Just (REAL pos, matched, restOfInput)
nextStaticToken (match_token "RECORD" -> Just (matched, restOfInput)) pos = Just (RECORD pos, matched, restOfInput)
nextStaticToken (match_token "REPEAT" -> Just (matched, restOfInput)) pos = Just (REPEAT pos, matched, restOfInput)
nextStaticToken (match_token "SET" -> Just (matched, restOfInput)) pos = Just (SET pos, matched, restOfInput)
nextStaticToken (match_token "THEN" -> Just (matched, restOfInput)) pos = Just (THEN pos, matched, restOfInput)
nextStaticToken (match_token "TO" -> Just (matched, restOfInput)) pos = Just (TO pos, matched, restOfInput)
nextStaticToken (match_token "TYPE" -> Just (matched, restOfInput)) pos = Just (TYPE pos, matched, restOfInput)
nextStaticToken (match_token "UNTIL" -> Just (matched, restOfInput)) pos = Just (UNTIL pos, matched, restOfInput)
nextStaticToken (match_token "VAR" -> Just (matched, restOfInput)) pos = Just (VAR pos, matched, restOfInput)
nextStaticToken (match_token "WHILE" -> Just (matched, restOfInput)) pos = Just (WHILE pos, matched, restOfInput)
nextStaticToken (match_token "WITH" -> Just (matched, restOfInput)) pos = Just (WITH pos, matched, restOfInput)
nextStaticToken (match_token "\\+" -> Just (matched, restOfInput)) pos = Just (PLUS pos, matched, restOfInput)
nextStaticToken (match_token "-" -> Just (matched, restOfInput)) pos = Just (MINUS pos, matched, restOfInput)
nextStaticToken (match_token "\\*" -> Just (matched, restOfInput)) pos = Just (STAR pos, matched, restOfInput)
nextStaticToken (match_token "/" -> Just (matched, restOfInput)) pos = Just (SLASH pos, matched, restOfInput)
nextStaticToken (match_token ":=" -> Just (matched, restOfInput)) pos = Just (ASSIGN pos, matched, restOfInput)
nextStaticToken (match_token "," -> Just (matched, restOfInput)) pos = Just (COMMA pos, matched, restOfInput)
nextStaticToken (match_token ";" -> Just (matched, restOfInput)) pos = Just (SEMI pos, matched, restOfInput)
nextStaticToken (match_token ":" -> Just (matched, restOfInput)) pos = Just (COLON pos, matched, restOfInput)
nextStaticToken (match_token "=" -> Just (matched, restOfInput)) pos = Just (EQUAL pos, matched, restOfInput)
nextStaticToken (match_token "<>" -> Just (matched, restOfInput)) pos = Just (NOT_EQUAL pos, matched, restOfInput)
nextStaticToken (match_token "<=" -> Just (matched, restOfInput)) pos = Just (LE pos, matched, restOfInput)
nextStaticToken (match_token "<" -> Just (matched, restOfInput)) pos = Just (LT_ pos, matched, restOfInput)
nextStaticToken (match_token ">=" -> Just (matched, restOfInput)) pos = Just (GE pos, matched, restOfInput)
nextStaticToken (match_token ">" -> Just (matched, restOfInput)) pos = Just (GT_ pos, matched, restOfInput)
nextStaticToken (match_token "\\(\\." -> Just (matched, restOfInput)) pos = Just (LBRACK2 pos, matched, restOfInput)
nextStaticToken (match_token "\\(" -> Just (matched, restOfInput)) pos = Just (LPAREN pos, matched, restOfInput)
nextStaticToken (match_token "\\)" -> Just (matched, restOfInput)) pos = Just (RPAREN pos, matched, restOfInput)
nextStaticToken (match_token "\\[" -> Just (matched, restOfInput)) pos = Just (LBRACK pos, matched, restOfInput)
nextStaticToken (match_token "\\]" -> Just (matched, restOfInput)) pos = Just (RBRACK pos, matched, restOfInput)
nextStaticToken (match_token "\\.\\)" -> Just (matched, restOfInput)) pos = Just (RBRACK2 pos, matched, restOfInput)
nextStaticToken (match_token "\\^" -> Just (matched, restOfInput)) pos = Just (POINTER pos, matched, restOfInput)
nextStaticToken (match_token "@" -> Just (matched, restOfInput)) pos = Just (AT pos, matched, restOfInput)
nextStaticToken (match_token "\\.\\." -> Just (matched, restOfInput)) pos = Just (DOTDOT pos, matched, restOfInput)
nextStaticToken (match_token "\\." -> Just (matched, restOfInput)) pos = Just (DOT pos, matched, restOfInput)
nextStaticToken (match_token "{" -> Just (matched, restOfInput)) pos = Just (LCURLY pos, matched, restOfInput)
nextStaticToken (match_token "}" -> Just (matched, restOfInput)) pos = Just (RCURLY pos, matched, restOfInput)
nextStaticToken (match_token "UNIT" -> Just (matched, restOfInput)) pos = Just (UNIT pos, matched, restOfInput)
nextStaticToken (match_token "USES" -> Just (matched, restOfInput)) pos = Just (USES pos, matched, restOfInput)
nextStaticToken (match_token "STRING" -> Just (matched, restOfInput)) pos = Just (STRING pos, matched, restOfInput)
nextStaticToken (match_token "IMPLEMENTATION" -> Just (matched, restOfInput)) pos = Just (IMPLEMENTATION pos, matched, restOfInput)
nextStaticToken _ _ = Nothing


-- | Produce dynamic token and the rest of the input.
nextDynamicToken :: [Char] -> Cursor -> Maybe (Token, [Char])
nextDynamicToken (match_token "\\(\\*(.|\r|\n)+?\\*\\)" -> Just (comment, restOfInput)) pos = Just (COMMENT_1 pos comment, restOfInput)
nextDynamicToken (match_token "{(.|\r|\n)+?}" -> Just (comment, restOfInput)) pos = Just (COMMENT_2 pos comment, restOfInput)
nextDynamicToken (match_token "'(''|.)+'" -> Just (str, restOfInput)) pos = Just (STRING_LITERAL pos str, restOfInput)
nextDynamicToken (match_token "''" -> Just (str, restOfInput)) pos = Just (STRING_LITERAL pos str, restOfInput)
nextDynamicToken (match_token "\\d+\\.\\d+(e(\\+|-)?\\d+)?" -> Just (num, restOfInput)) pos = Just (NUM_INT pos num, restOfInput)
nextDynamicToken (match_token "\\d+(e(\\+|\\-)?\\d+)?" -> Just (num, restOfInput)) pos = Just (NUM_INT pos num, restOfInput)
nextDynamicToken (match_token "[ \t\r\n]+" -> Just (ws, restOfInput)) pos = Just (WS pos ws, restOfInput)
nextDynamicToken (match_token "[a-z]([a-z]|[0-9]|_)*" -> Just (ident, restOfInput)) pos = Just (IDENT pos ident, restOfInput)
nextDynamicToken _ _ = Nothing


-- | Produce next token. Either dynamic or static. And the rest of the input.
-- Result is Maybe (token, matched input, rest of input)
nextToken :: [Char] -> Cursor -> Maybe (Token, [Char], [Char])
nextToken input pos = let (staticMatch, dynamicMatch) = (nextStaticToken input pos, nextDynamicToken input pos)
  in case (staticMatch, dynamicMatch) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just (token, restOfInput)) -> Just (token, tokenValue token, restOfInput)
    (Just (token, match, restOfInput), Nothing) -> Just (token, match, restOfInput)
    (Just (stoken, smatch, srestOfInput), Just (dtoken, drestOfInput)) ->
      if length smatch < length (tokenValue dtoken)
        then Just (dtoken, tokenValue dtoken, drestOfInput)
        else Just (stoken, smatch, srestOfInput)


-- | Produce list of tokens for an input. Or fail with error.
tokenize :: [Char] -> [Token]
tokenize [] = [EOF]
tokenize input = let tokenFound = nextToken input (Cursor 0 0)
  in case tokenFound of
    Nothing -> error ("Can't produce read: " ++ take 20 input)
    Just (token, _, restOfInput) -> [token] ++ tokenize restOfInput
