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
    Nothing -> error (errMsg input cur)
    Just (token, matched, restOfInput) -> [token] ++ _tokenize restOfInput (moveCursor cur matched)


-- | Format nice error message.
errMsg restOfInput cursor = "Can't read at line/col [" ++
                            show (lineNumber cursor) ++
                            ", " ++
                            show (column cursor) ++
                            "] -> " ++
                            take 20 restOfInput
