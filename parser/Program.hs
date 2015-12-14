module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving Show

statements = iter Statement.parse >-> buildStatements
buildStatements :: [Statement.T] -> T
buildStatements = Program

instance Parse T where
  parse = statements
  toString = toString'

exec (Program stmts) = Statement.exec stmts Dictionary.empty
toString' (Program stmts) = concat $ map Statement.toString stmts
