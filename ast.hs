-- | AST structures

module AST where


data Program = Program ProgramHeading Bool Block
             deriving (Show, Eq)

data ProgramHeading = ProgramHeading String (Maybe IdentifierList)
                    | UnitHeading String
                    deriving(Show, Eq)

data Block = Block CompoundStatement
           deriving(Show, Eq)

data CompoundStatement = CompoundStatement Statements
                       deriving(Show, Eq)


data Statement = ProcedureStatement String [Parameter]  -- name and params
              deriving (Show, Eq)

data Parameter = StringParameter String
              deriving (Show, Eq)

type Statements = [Statement]
type IdentifierList = [String]
