-- | Parser to produce abstract syntax tree.

module Parser where

import LexerTokens


-- Program's structures for AST.

-- Program is heading, flag if is interface and block.
data Program = Program ProgramHeading Bool Block
             deriving (Show, Eq)

data ProgramHeading = ProgramHeading String (Maybe IdentifierList)
                    | UnitHeading String
                    deriving(Show, Eq)

data Block = Block  -- wip
           deriving(Show, Eq)

data IdentifierList = IdentifierList [String]
                    deriving (Show, Eq)

idList :: IdentifierList -> [String]
idList (IdentifierList values) = values

idListPrepend :: String -> IdentifierList -> IdentifierList
idListPrepend name lst = IdentifierList ([name] ++ idList lst)


-- | Parse a program.
parse :: [Token] -> Program
parse input = let (p, restOfInput) = mustBe "Program declaration" (filterOut input) parseProgram
  in case head restOfInput of
    EOF -> p
    otherwise -> tokenExpectationError "End of input" (head restOfInput)


-- Parsing elements.


          -- program
          --    : programHeading (INTERFACE)? block DOT
          --    ;
parseProgram :: [Token] -> Maybe (Program, [Token])
parseProgram input = let (pHead, restOfInput) = mustBe "Program or Unit" input parseProgramHeading
  in let (interfaceFlag,deepRest) = maybeToken "INTERFACE" restOfInput
    in let (block, deepestRest) = mustBe "Block" deepRest parseBlock
      in Just (Program pHead interfaceFlag block, deepestRest)


parseProgramHeading :: [Token] -> Maybe (ProgramHeading, [Token])
parseProgramHeading (UNIT _: restOfInput) = let (name, rest) = parseIdentifier restOfInput
  in Just (UnitHeading name, semiExpected rest)
parseProgramHeading (PROGRAM _: restOfInput) = let (name, rest) = parseIdentifier restOfInput
  in case head rest of
    LPAREN _ -> let (values, deepRest) = parseIdentifierList(tail rest)
              in Just ((ProgramHeading name (Just values), semiExpected (rparenExpected deepRest)))
    otherwise -> Just (ProgramHeading name Nothing, semiExpected rest)
parseProgramHeading (x:_) = Nothing

parseBlock :: [Token] -> Maybe (Block, [Token])
parseBlock input = Just (Block, input)


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

-- common stuff

-- | Filter out comments and whitespaces
filterOut :: [Token] -> [Token]
filterOut (WS _ _:restOfInput) = filterOut restOfInput
filterOut (COMMENT_1 _ _:restOfInput) = filterOut restOfInput
filterOut (COMMENT_2 _ _:restOfInput) = filterOut restOfInput
filterOut (t:restOfInput) = [t] ++ filterOut restOfInput
filterOut [] = []


mustBe :: String -> [Token ] -> ([Token] -> Maybe (a, [Token])) -> (a, [Token])
mustBe what input f = let r = f input
  in case r of
    Nothing -> tokenExpectationError what (head input)
    Just (e, restOfInput) -> (e, restOfInput)


maybeToken :: [Char] -> [Token] -> (Bool, [Token])
maybeToken name inp@(x:restOfInput) = if name == tokenName x
  then (True, restOfInput)
  else (False, inp)


tokenExpectationError :: String -> Token -> b
tokenExpectationError tokenNameExpected token = case token of
  EOF -> error (tokenNameExpected ++ " expected but end of file reached")
  otherwise -> error (tokenNameExpected ++" expected at "++ show(cursorValue(token)) ++ ", but it's " ++ (tokenName token))
