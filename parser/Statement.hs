module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail, read)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip |
    Block [Statement] |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = (accept "skip" # require ";") >-> buildSkip
buildSkip _ = Skip

block = accept "begin" -# iter statement #- require "end" >-> buildBlock
buildBlock ss = Block ss

if_ = accept "if" -# Expr.parse #- require "then" # (statement #- require "else" # statement) >-> buildIf
buildIf (ex, (th, el)) = If ex th el

while = accept "while" -# Expr.parse #- require "do" # statement >-> buildWhile
buildWhile (e, s) = While e s

read = accept "read" #- word #- require ";" >-> buildRead
buildRead v = Read v

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite v = Write v

statement = assignment ! skip ! block ! if_ ! while ! read ! write

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = statement
  toString = error "Statement.toString not implemented"
