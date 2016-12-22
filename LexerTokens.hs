-- | Pascal lang tokens.

module LexerTokens where


-- | Position of a token in an input; line and column.
data Cursor = Cursor Int Int
  deriving(Eq)


-- | Show for Cursor.
instance Show Cursor where
  show (Cursor a b) = "{Ln: " ++ (show a) ++ ", Col: " ++ (show b) ++ "}"


-- | Get line number from a position.
lineNumber :: Cursor -> Int
lineNumber (Cursor n _) = n


-- | Get Column number from a position.
column :: Cursor -> Int
column (Cursor _ n) = n


-- | Moves cursor forward to new position.
moveCursor :: Cursor -> [Char] -> Cursor
moveCursor (Cursor l c) []  = Cursor l c
moveCursor p@(Cursor l c) ('\r':xs)  = moveCursor p xs
moveCursor (Cursor l c) ('\n':xs)  = moveCursor (Cursor (l + 1) 1) xs
moveCursor (Cursor l c) (_:xs)  = moveCursor (Cursor l (c + 1)) xs


-- | Valid tokens for Pascal lang.
-- As defined in res/pascal.g4
data Token = EOF  -- End of file/input
           | AND Cursor  -- A N D
           | ARRAY Cursor  -- A R R A Y
           | BEGIN Cursor  -- B E G I N
           | BOOLEAN Cursor  -- B O O L E A N
           | CASE Cursor  -- C A S E
           | CHAR Cursor  -- C H A R
           | CHR Cursor  -- C H R
           | CONST Cursor  -- C O N S T
           | DIV Cursor  -- D I V
           | DO Cursor  -- D O
           | DOWNTO Cursor  -- D O W N T O
           | ELSE Cursor  -- E L S E
           | END Cursor  -- E N D
           | FILE Cursor  -- F I L E
           | FOR Cursor  -- F O R
           | FUNCTION Cursor  -- F U N C T I O N
           | GOTO Cursor  -- G O T O
           | IF Cursor  -- I F
           | IN Cursor  -- I N
           | INTEGER Cursor  -- I N T E G E R
           | LABEL Cursor  -- L A B E L
           | MOD Cursor  -- M O D
           | NIL Cursor  -- N I L
           | NOT Cursor  -- N O T
           | OF Cursor  -- O F
           | OR Cursor  -- O R
           | PACKED Cursor  -- P A C K E D
           | PROCEDURE Cursor  -- P R O C E D U R E
           | PROGRAM Cursor  -- P R O G R A M
           | REAL Cursor  -- R E A L
           | RECORD Cursor  -- R E C O R D
           | REPEAT Cursor  -- R E P E A T
           | SET Cursor  -- S E T
           | THEN Cursor  -- T H E N
           | TO Cursor  -- T O
           | TYPE Cursor  -- T Y P E
           | UNTIL Cursor  -- U N T I L
           | VAR Cursor  -- V A R
           | WHILE Cursor  -- W H I L E
           | WITH Cursor  -- W I T H
           | PLUS Cursor  -- '+'
           | MINUS Cursor  -- '-'
           | STAR Cursor  -- '*'
           | SLASH Cursor  -- '/'
           | ASSIGN Cursor  -- ':='
           | COMMA Cursor  -- ','
           | SEMI Cursor  -- ';'
           | COLON Cursor  -- ':'
           | EQUAL Cursor  -- '='
           | NOT_EQUAL Cursor  -- '<>'
           | LT_ Cursor  -- '<'
           | LE Cursor  -- '<='
           | GE Cursor  -- '>='
           | GT_ Cursor  -- '>'
           | LPAREN Cursor  -- '('
           | RPAREN Cursor  -- ')'
           | LBRACK Cursor  -- '['
           | LBRACK2 Cursor  -- '(.'
           | RBRACK Cursor  -- ']'
           | RBRACK2 Cursor  -- '.)'
           | POINTER Cursor  -- '^'
           | AT Cursor  -- '@'
           | DOT Cursor  -- '.'
           | DOTDOT Cursor  -- '..'
           | LCURLY Cursor  -- '{'
           | RCURLY Cursor  -- '}'
           | UNIT Cursor  -- U N I T
           | INTERFACE Cursor  -- I N T E R F A C E
           | USES Cursor  -- U S E S
           | STRING Cursor  -- S T R I N G
           | IMPLEMENTATION Cursor  -- I M P L E M E N T A T I O N
           | WS Cursor String  -- [ \t\r\n] -> skip
           | COMMENT_1 Cursor String  -- '(*' .*? '*)' -> skip
           | COMMENT_2 Cursor String  -- '{' .*? '}' -> skip
           | IDENT Cursor String  -- ('a' .. 'z' | 'A' .. 'Z') ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_')*
           | STRING_LITERAL Cursor String  -- '\'' ('\'\'' | ~ ('\''))* '\''
           | NUM_INT Cursor String  -- ('0' .. '9') + (('.' ('0' .. '9') + (EXPONENT)?)? | EXPONENT)
           deriving (Show, Eq)


-- | Extract value from a dynamic token.
tokenValue :: Token -> String
tokenValue(WS _ s) = s
tokenValue(COMMENT_1 _ s) = s
tokenValue(COMMENT_2 _ s) = s
tokenValue(IDENT _ s) = s
tokenValue(STRING_LITERAL _ s) = s
tokenValue(NUM_INT _ s) = s


-- | Extract cursor from a token
cursorValue :: Token -> Cursor
cursorValue (AND c) = c
cursorValue (ARRAY c) = c
cursorValue (BEGIN c) = c
cursorValue (BOOLEAN c) = c
cursorValue (CASE c) = c
cursorValue (CHAR c) = c
cursorValue (CHR c) = c
cursorValue (CONST c) = c
cursorValue (DIV c) = c
cursorValue (DO c) = c
cursorValue (DOWNTO c) = c
cursorValue (ELSE c) = c
cursorValue (END c) = c
cursorValue (FILE c) = c
cursorValue (FOR c) = c
cursorValue (FUNCTION c) = c
cursorValue (GOTO c) = c
cursorValue (IF c) = c
cursorValue (IN c) = c
cursorValue (INTEGER c) = c
cursorValue (LABEL c) = c
cursorValue (MOD c) = c
cursorValue (NIL c) = c
cursorValue (NOT c) = c
cursorValue (OF c) = c
cursorValue (OR c) = c
cursorValue (PACKED c) = c
cursorValue (PROCEDURE c) = c
cursorValue (PROGRAM c) = c
cursorValue (REAL c) = c
cursorValue (RECORD c) = c
cursorValue (REPEAT c) = c
cursorValue (SET c) = c
cursorValue (THEN c) = c
cursorValue (TO c) = c
cursorValue (TYPE c) = c
cursorValue (UNTIL c) = c
cursorValue (VAR c) = c
cursorValue (WHILE c) = c
cursorValue (WITH c) = c
cursorValue (PLUS c) = c
cursorValue (MINUS c) = c
cursorValue (STAR c) = c
cursorValue (SLASH c) = c
cursorValue (ASSIGN c) = c
cursorValue (COMMA c) = c
cursorValue (SEMI c) = c
cursorValue (COLON c) = c
cursorValue (EQUAL c) = c
cursorValue (NOT_EQUAL c) = c
cursorValue (LT_ c) = c
cursorValue (LE c) = c
cursorValue (GE c) = c
cursorValue (GT_ c) = c
cursorValue (LPAREN c) = c
cursorValue (RPAREN c) = c
cursorValue (LBRACK c) = c
cursorValue (LBRACK2 c) = c
cursorValue (RBRACK c) = c
cursorValue (RBRACK2 c) = c
cursorValue (POINTER c) = c
cursorValue (AT c) = c
cursorValue (DOT c) = c
cursorValue (DOTDOT c) = c
cursorValue (LCURLY c) = c
cursorValue (RCURLY c) = c
cursorValue (UNIT c) = c
cursorValue (INTERFACE c) = c
cursorValue (USES c) = c
cursorValue (STRING c) = c
cursorValue (IMPLEMENTATION c) = c
cursorValue (WS c _) = c
cursorValue (COMMENT_1 c _) = c
cursorValue (COMMENT_2 c _) = c
cursorValue (IDENT c _) = c
cursorValue (STRING_LITERAL c _) = c
cursorValue (NUM_INT c _) = c
