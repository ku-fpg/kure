module ExprLanguage where

data Cmd  = Seq Cmd Cmd | Assign Name Expr
data Expr = Var Name | Lit Int | Add Expr Expr | ESeq Cmd Expr

type Name = String

newtype Ctxt a = Ctxt (a,[(Name,Expr)])
