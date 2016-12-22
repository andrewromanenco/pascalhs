-- | Parser to produce abstract syntax tree.

module Parser where

import LexerTokens


-- Program's structures for AST.
data ProgramHeading = ProgramHeading String (Maybe IdentifierList)
                    | UnitHeading String
                    deriving(Show, Eq)

data IdentifierList = IdentifierList [String]
                    deriving (Show, Eq)

idList :: IdentifierList -> [String]
idList (IdentifierList values) = values

idListPrepend :: String -> IdentifierList -> IdentifierList
idListPrepend name lst = IdentifierList ([name] ++ idList lst)

-- Parsing elements.

parseProgramHeading :: [Token] -> (ProgramHeading, [Token])
parseProgramHeading (UNIT _: restOfInput) = let (name, rest) = parseIdentifier restOfInput
  in (UnitHeading name, semiExpected rest)
parseProgramHeading (PROGRAM _: restOfInput) = let (name, rest) = parseIdentifier restOfInput
  in case head rest of
    LPAREN _ -> let (values, deepRest) = parseIdentifierList(tail rest)
              in ((ProgramHeading name (Just values), semiExpected (rparenExpected deepRest)))
    otherwise -> (ProgramHeading name Nothing, semiExpected rest)
parseProgramHeading (x:_) = tokenExpectationError "Program or unit declaration" x


parseIdentifierList :: [Token] -> (IdentifierList, [Token])
parseIdentifierList input@(IDENT _ _:_) = let (name, restOfInput) = parseIdentifier input
  in case head restOfInput of
    COMMA _ -> let (list, rest) = parseIdentifierList(tail restOfInput)
               in (idListPrepend name list, rest)
    otherwise -> (IdentifierList [name], restOfInput)
parseIdentifierList (x:_) = tokenExpectationError "Identifier" x


parseIdentifier :: [Token] -> (String, [Token])
parseIdentifier (IDENT _ name:restOfInput) = (name, restOfInput)
parseIdentifier (x:_) = tokenExpectationError "Identifier" x

semiExpected :: [Token] -> [Token]
semiExpected (SEMI _:restOfInput) = restOfInput
semiExpected (x:_) = tokenExpectationError "Semicolon" x

rparenExpected :: [Token] -> [Token]
rparenExpected (RPAREN _:restOfInput) = restOfInput
rparenExpected (x:_) = tokenExpectationError "RPAREN" x


tokenExpectationError :: String -> Token -> a
tokenExpectationError tokenNameExpected token = case token of
  EOF -> error (tokenNameExpected ++ " expected but end of file reached")
  otherwise -> error (tokenNameExpected ++" expected at "++ show(cursorValue(token)))
