module Expr where

-----------------------------------------------------------------

type Name = String

data Cmd  = Seq Cmd Cmd | Assign Name Expr
            deriving Eq

data Expr = Var Name | Lit Int | Add Expr Expr | ESeq Cmd Expr
            deriving Eq

instance Show Cmd where
  show (Seq c1 c2) = show c1 ++ " ; " ++ show c2
  show (Assign n e) = n ++ " := " ++ show e

instance Show Expr where
  show (Var n)     = n
  show (Lit n)     = show n
  show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (ESeq c e)  = "\n( let " ++ show c ++ "\n   in " ++ show e ++ ")\n"

-----------------------------------------------------------------

type Context = [(Name,Expr)]

updateContext :: Cmd -> Context -> Context
updateContext (Seq c1 c2)  = updateContext c2 . updateContext c1
updateContext (Assign v e) = ((v,e):)

-----------------------------------------------------------------
