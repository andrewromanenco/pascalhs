-- | AST interpreter.

module Interpreter where

import Data.Char
import AST

class Interpret a where
  interpret :: a -> IO ()

instance Interpret Program where
  interpret (Program heading _ block) = do
    interpret heading
    interpret block

instance Interpret ProgramHeading where
  interpret (ProgramHeading name _) = putStrLn ("Running program: " ++ name)
  interpret (UnitHeading name) = error "Can't run a pascal Unit (must be a program)."

instance Interpret Block where
  interpret (Block cstates) = do
    interpret cstates

instance Interpret CompoundStatement where
  interpret (CompoundStatement statements) = do
    mapM interpret statements
    return ()

instance Interpret Statement where
  interpret (ProcedureStatement procName params) = do
    case map toLower procName of
      "write" -> pWrite params
      "readln" -> pReadLn params
      otherwise -> error ("Proc not yet implemented: " ++ procName)


pWrite :: [Parameter] -> IO ()
pWrite [StringParameter value] = putStr value
pWrite _ = error "Non string or multipe params are not yet supported"


pReadLn :: [Parameter] -> IO ()
pReadLn _ = do
  _ <- getLine
  return ()
