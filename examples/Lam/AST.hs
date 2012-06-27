module Lam.AST where

-------------------------------------------------------------------------------

type Name = String

data Exp = Lam Name Exp
         | App Exp Exp
         | Var Name
           deriving Eq

instance Show Exp where
  show (Var v)   = v
  show (App x y) = "(" ++ show x ++ " " ++ show y ++ ")"
  show (Lam n x) = "(\\" ++ n ++ ". " ++ show x ++ ")"

-------------------------------------------------------------------------------
