module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T]

statements = iter Statement.parse >-> buildStatements
buildStatements = Program

instance Parse T where
  parse = statements
  toString = error "Program.toString not implemented"

exec = error "Program.exec not implemented"
