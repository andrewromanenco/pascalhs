-- | This module takes input string and splits it into list of tokens.

module Lexer ( tokenize ) where

import LexerTokens
import LexerRules


-- | Produce list of tokens for an input. Or fail with error.
tokenize :: [Char] -> [Token]
tokenize input = _tokenize input (Cursor 1 1)


-- | Produce list with tracking cursor position.
_tokenize :: [Char] -> Cursor -> [Token]
_tokenize [] _ = [EOF]
_tokenize input cur = let tokenFound = nextToken input cur
  in case tokenFound of
    Nothing -> error ("Can't produce read: " ++ take 20 input)
    Just (token, matched, restOfInput) -> [token] ++ _tokenize restOfInput (moveCursor cur matched)
