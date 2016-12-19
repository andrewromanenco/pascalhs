-- | Rules for tokens.

{-# LANGUAGE ViewPatterns #-}

module LexerRules where

import LexerTokens
import Utils


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
