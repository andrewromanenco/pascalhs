-- | Parser to produce abstract syntax tree.

module Parser where

import LexerTokens


-- Program's structures for AST.
data ProgramHeading = ProgramHeading String
                    | UnitHeading String
                    deriving(Show, Eq)


-- Parsing elements.

parseProgramHeading :: [Token] -> Maybe (ProgramHeading, [Token])
parseProgramHeading (UNIT _: restOfInput) = let mName = parseIdentifier restOfInput
  in case mName of
    Nothing -> error "Can't find name identifier for UNIT"
    Just (name, restOfInput) -> Just (UnitHeading name, semiExpected restOfInput)
parseProgramHeading _ = Nothing

parseIdentifier :: [Token] -> Maybe (String, [Token])
parseIdentifier (IDENT _ name:restOfInput) = Just (name, restOfInput)
parseIdentifier _ = Nothing

semiExpected :: [Token] -> [Token]
semiExpected (SEMI _:restOfInput) = restOfInput
semiExpected (EOF:_) = error "Semicolon expected, but end of file reached."
semiExpected (x:_) = error ("Semicolon expected, but not found at " ++ show(cursorValue(x)))
