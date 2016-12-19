-- | This module takes input string and splits it into list of tokens.

module Lexer ( tokenize ) where

import LexerTokens
import LexerRules


-- | Produce list of tokens for an input. Or fail with error.
tokenize :: [Char] -> [Token]
tokenize [] = [EOF]
tokenize input = let tokenFound = nextToken input (Cursor 0 0)
  in case tokenFound of
    Nothing -> error ("Can't produce read: " ++ take 20 input)
    Just (token, _, restOfInput) -> [token] ++ tokenize restOfInput
