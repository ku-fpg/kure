module ExprLanguage where

data Cmd  = Seq Cmd Cmd | Assign Name Expr
            deriving Show
                     
data Expr = Var Name | Lit Int | Add Expr Expr | ESeq Cmd Expr
            deriving Show

type Name = String

type Context = [(Name,Expr)]

-- for simplicity we assume the language does not allow variable shadowing

updateContext :: Cmd -> Context -> Context
updateContext (Seq c1 c2)  = updateContext c2 . updateContext c1
updateContext (Assign v e) = ((v,e):)