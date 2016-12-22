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

data Block = Block CompoundStatement
           deriving(Show, Eq)

data CompoundStatement = CompoundStatement Statements
                       deriving(Show, Eq)


data Statement = Statement
              deriving (Show, Eq)

type Statements = [Statement]
type IdentifierList = [String]

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
      in Just (Program pHead interfaceFlag block, mustBeToken "DOT" deepestRest)


parseProgramHeading :: [Token] -> Maybe (ProgramHeading, [Token])
parseProgramHeading (UNIT _: restOfInput) = let (name, rest) = parseIdentifier restOfInput
  in Just (UnitHeading name, semiExpected rest)
parseProgramHeading (PROGRAM _: restOfInput) = let (name, rest) = parseIdentifier restOfInput
  in case head rest of
    LPAREN _ -> let (values, deepRest) = parseIdentifierList(tail rest)
              in Just ((ProgramHeading name (Just values), semiExpected (rparenExpected deepRest)))
    otherwise -> Just (ProgramHeading name Nothing, semiExpected rest)
parseProgramHeading (x:_) = Nothing


          -- block
          --    : (labelDeclarationPart | constantDefinitionPart | typeDefinitionPart | variableDeclarationPart | procedureAndFunctionDeclarationPart | usesUnitsPart | IMPLEMENTATION)* compoundStatement
          --    ;
parseBlock :: [Token] -> Maybe (Block, [Token])
parseBlock input = let (cStatement, restOfInput) = mustBe "BEGIN block" input parseCompoundStatement
  in Just(Block cStatement, restOfInput)

          -- compoundStatement
          --    : BEGIN statements END
          --    ;
parseCompoundStatement :: [Token] -> Maybe (CompoundStatement, [Token])
parseCompoundStatement input = let (statements, restOfInput) = mustBe "Statement" (mustBeToken "BEGIN" input) parseStatements
  in Just (CompoundStatement statements, mustBeToken "END" restOfInput)

parseStatements :: [Token] -> Maybe (Statements, [Token])
parseStatements input = Just ([], input)

parseIdentifierList :: [Token] -> (IdentifierList, [Token])
parseIdentifierList input@(IDENT _ _:_) = let (name, restOfInput) = parseIdentifier input
  in case head restOfInput of
    COMMA _ -> let (list, rest) = parseIdentifierList(tail restOfInput)
               in ([name] ++ list, rest)
    otherwise -> ([name], restOfInput)
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


mustBe :: String -> [Token] -> ([Token] -> Maybe (a, [Token])) -> (a, [Token])
mustBe what input f = let r = f input
  in case r of
    Nothing -> tokenExpectationError what (head input)
    Just (e, restOfInput) -> (e, restOfInput)


maybeToken :: [Char] -> [Token] -> (Bool, [Token])
maybeToken name inp@(x:restOfInput) = if name == tokenName x
  then (True, restOfInput)
  else (False, inp)


mustBeToken :: [Char] -> [Token] -> [Token]
mustBeToken name inp@(t:restOfInput) = if name == tokenName t
  then restOfInput
  else tokenExpectationError name (head inp)


tokenExpectationError :: String -> Token -> b
tokenExpectationError tokenNameExpected token = case token of
  EOF -> error (tokenNameExpected ++ " expected but end of file reached")
  otherwise -> error (tokenNameExpected ++" expected at "++ show(cursorValue(token)) ++ ", but it's " ++ (tokenName token))
