-- | This module takes input string and splits it into list of tokens.

{-# LANGUAGE ViewPatterns #-}

module Lexer where

import Data.List
import Data.Maybe
import Text.Regex.PCRE


-- | Position in input text; line and column.
data Pos = Pos Int Int
  deriving(Eq)


-- | Show for Pos.
instance Show Pos where
  show (Pos a b) = "{Ln: " ++ (show a) ++ ", Col: " ++ (show b) ++ "}"


-- | Get line number from a position.
lineNumber :: Pos -> Int
lineNumber (Pos n _) = n


-- | Get Column number from a position.
column :: Pos -> Int
column (Pos _ n) = n


-- | Moves cursor forward to new position.
moveCursor :: Pos -> [Char] -> Pos
moveCursor (Pos l c) []  = Pos l c
moveCursor p@(Pos l c) ('\r':xs)  = moveCursor p xs
moveCursor (Pos l c) ('\n':xs)  = moveCursor (Pos (l + 1) 1) xs
moveCursor (Pos l c) (_:xs)  = moveCursor (Pos l (c + 1)) xs


-- | Valid tokens for Pascal lang.
-- As defined in res/pascal.g4
data Token = EOF  -- End of file/input
           | AND Pos  -- A N D
           | ARRAY Pos  -- A R R A Y
           | BEGIN Pos  -- B E G I N
           | BOOLEAN Pos  -- B O O L E A N
           | CASE Pos  -- C A S E
           | CHAR Pos  -- C H A R
           | CHR Pos  -- C H R
           | CONST Pos  -- C O N S T
           | DIV Pos  -- D I V
           | DO Pos  -- D O
           | DOWNTO Pos  -- D O W N T O
           | ELSE Pos  -- E L S E
           | END Pos  -- E N D
           | FILE Pos  -- F I L E
           | FOR Pos  -- F O R
           | FUNCTION Pos  -- F U N C T I O N
           | GOTO Pos  -- G O T O
           | IF Pos  -- I F
           | IN Pos  -- I N
           | INTEGER Pos  -- I N T E G E R
           | LABEL Pos  -- L A B E L
           | MOD Pos  -- M O D
           | NIL Pos  -- N I L
           | NOT Pos  -- N O T
           | OF Pos  -- O F
           | OR Pos  -- O R
           | PACKED Pos  -- P A C K E D
           | PROCEDURE Pos  -- P R O C E D U R E
           | PROGRAM Pos  -- P R O G R A M
           | REAL Pos  -- R E A L
           | RECORD Pos  -- R E C O R D
           | REPEAT Pos  -- R E P E A T
           | SET Pos  -- S E T
           | THEN Pos  -- T H E N
           | TO Pos  -- T O
           | TYPE Pos  -- T Y P E
           | UNTIL Pos  -- U N T I L
           | VAR Pos  -- V A R
           | WHILE Pos  -- W H I L E
           | WITH Pos  -- W I T H
           | PLUS Pos  -- '+'
           | MINUS Pos  -- '-'
           | STAR Pos  -- '*'
           | SLASH Pos  -- '/'
           | ASSIGN Pos  -- ':='
           | COMMA Pos  -- ','
           | SEMI Pos  -- ';'
           | COLON Pos  -- ':'
           | EQUAL Pos  -- '='
           | NOT_EQUAL Pos  -- '<>'
           | LT_ Pos  -- '<'
           | LE Pos  -- '<='
           | GE Pos  -- '>='
           | GT_ Pos  -- '>'
           | LPAREN Pos  -- '('
           | RPAREN Pos  -- ')'
           | LBRACK Pos  -- '['
           | LBRACK2 Pos  -- '(.'
           | RBRACK Pos  -- ']'
           | RBRACK2 Pos  -- '.)'
           | POINTER Pos  -- '^'
           | AT Pos  -- '@'
           | DOT Pos  -- '.'
           | DOTDOT Pos  -- '..'
           | LCURLY Pos  -- '{'
           | RCURLY Pos  -- '}'
           | UNIT Pos  -- U N I T
           | INTERFACE Pos  -- I N T E R F A C E
           | USES Pos  -- U S E S
           | STRING Pos  -- S T R I N G
           | IMPLEMENTATION Pos  -- I M P L E M E N T A T I O N
           | WS Pos String  -- [ \t\r\n] -> skip
           | COMMENT_1 Pos String  -- '(*' .*? '*)' -> skip
           | COMMENT_2 Pos String  -- '{' .*? '}' -> skip
           | IDENT Pos String  -- ('a' .. 'z' | 'A' .. 'Z') ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_')*
           | STRING_LITERAL Pos String  -- '\'' ('\'\'' | ~ ('\''))* '\''
           | NUM_INT Pos String  -- ('0' .. '9') + (('.' ('0' .. '9') + (EXPONENT)?)? | EXPONENT)
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
nextStaticToken :: [Char] -> Pos -> Maybe (Token, [Char], [Char])
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
nextDynamicToken :: [Char] -> Pos -> Maybe (Token, [Char])
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
nextToken :: [Char] -> Pos -> Maybe (Token, [Char], [Char])
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
tokenize input = let tokenFound = nextToken input (Pos 0 0)
  in case tokenFound of
    Nothing -> error ("Can't produce read: " ++ take 20 input)
    Just (token, _, restOfInput) -> [token] ++ tokenize restOfInput
