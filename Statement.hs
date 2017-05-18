module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip |
    Begin [Statement] |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Comment String
    deriving Show

assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss :: (String, Expr.T) -> Statement
buildAss (v, e) = Assignment v e


ifstatement  :: Parser Statement
ifstatement = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf :: ((Expr.T, Statement), Statement) -> Statement
buildIf ((e, s1), s2) = If e s1 s2


skip :: Parser Statement
skip = accept "skip" #- require ";" >-> (\x -> Skip)

begin :: Parser Statement
begin = accept "begin" -# (iter parse) #- require "end" >-> buildBegin
buildBegin :: [Statement] -> Statement
buildBegin ss = Begin ss

while :: Parser Statement
while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile :: (Expr.T, Statement) -> Statement
buildWhile (e, s) = While e s

readVar :: Parser Statement
readVar = accept "read" -# word #- require ";" >-> Read

write :: Parser Statement
write = accept "write" -# Expr.parse #- require ";" >-> Write

comment :: Parser Statement
comment = accept "--" -# line #- require "\n" >-> Comment



exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Assignment var expr : stmts) dict input = exec stmts updatedDict input
  where updatedDict = Dictionary.insert (var, Expr.value expr dict) dict
exec (Skip : stmts) dict input = exec stmts dict input
exec (Begin ss : stmts) dict input = exec (ss ++ stmts) dict input
exec (While cond s : stmts) dict input
  | (Expr.value cond dict) > 0 = exec (s : (While cond s : stmts)) dict input
  | otherwise                  = exec stmts dict input
exec (Read var : stmts) dict (i:is) = exec stmts updatedDict is
  where updatedDict = Dictionary.insert (var, i) dict
exec (Write expr : stmts) dict input = (Expr.value expr dict) : (exec stmts dict input)
exec (Comment _ : stmts) dict input = exec stmts dict input


addTabs = flip replicate '\t'

-- Takes a level of indentation and a statement and makes a pretty string.
shw :: Int -> Statement -> String
shw i (Assignment var e) =  addTabs i ++ var ++ " := " ++ Expr.toString e ++ ";\n"
shw i (If e if_s then_s) =  addTabs i ++ "if " ++ Expr.toString e ++ " then\n"
                         ++ shw (i+1) if_s
                         ++ addTabs i ++ "else\n"
                         ++ shw (i+1) then_s
shw i Skip               =  addTabs i ++ "skip;\n"
shw i (Begin s_list)     =  addTabs i ++ "begin\n"
                         ++ concat (map (shw (i+1)) s_list)
                         ++ addTabs i ++ "end\n"
shw i (While e s)        =  addTabs i ++ "while " ++ Expr.toString e ++ " do\n"
                         ++ shw (i+1) s
shw i (Read s)           =  addTabs i ++ "read " ++ s ++ ";\n"
shw i (Write e)          =  addTabs i ++ "write " ++ Expr.toString e ++ ";\n"
shw i (Comment s)        =  addTabs i ++ "-- " ++ s ++ "\n"



instance Parse Statement where
  parse = assignment ! ifstatement ! skip ! begin ! while ! readVar ! write ! comment
  toString = shw 0
