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
buildIf (cond, (th, el)) = If cond th el

while = accept "while" -# Expr.parse #- require "do" # statement >-> buildWhile
buildWhile (e, s) = While e s

read = accept "read" -# word #- require ";" >-> buildRead
buildRead v = Read v

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite v = Write v

statement = assignment ! skip ! block ! if_ ! while ! read ! write

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Assignment var expr : stmts) dict input =
    exec stmts (Dictionary.insert (var, Expr.value expr dict) dict) input
exec (Skip : stmts) dict input = exec stmts dict input
exec (Block block : stmts) dict input = exec (block++stmts) dict input
exec (If cond thenStmts elseStmts : stmts) dict input =
    if (Expr.value cond dict)>0
        then exec (thenStmts : stmts) dict input
        else exec (elseStmts : stmts) dict input
exec loop@(While cond body : after) dict input =
    if (Expr.value cond dict)>0
        then exec (body : loop) dict input
        else exec after dict input
exec (Read var : stmts) dict (input:rest) =
    exec stmts (Dictionary.insert (var, input) dict) rest
exec (Write expr : stmts) dict input =
    Expr.value expr dict : exec stmts dict input
instance Parse Statement where
  parse = statement
  toString = toString'

toString' :: Statement -> String
toString' (Assignment var expr) = var ++ ":=" ++ toString expr ++ ";\n"
toString' Skip = "skip;\n"
toString' (Block block) = "begin\n" ++ concat (map toString' block) ++ "end\n"
toString' (If cond th el) = "if " ++ Expr.toString cond ++ " then\n"
    ++ toString' th ++ "else\n" ++ toString' el
toString' (While cond body) = "while " ++ Expr.toString cond ++ " do\n"
    ++ toString' body
toString' (Read var) = "read " ++ var ++ ";\n"
toString' (Write expr) = "write " ++ Expr.toString expr ++ ";\n"
