module Expr.AST where

import Language.KURE

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

data Context = Context AbsolutePath [(Name,Expr)]

instance PathContext Context where
  contextPath (Context p _) = p

addDef :: Name -> Expr -> Context -> Context
addDef v e (Context p defs) = Context p ((v,e):defs)

updateContextCmd :: Cmd -> Context -> Context
updateContextCmd (Seq c1 c2)  = updateContextCmd c2 . updateContextCmd c1
updateContextCmd (Assign v e) = (addDef v e)

(@@) :: Context -> Int -> Context
(Context p defs) @@ n = Context (extendAbsPath n p) defs

initialContext :: Context
initialContext = Context rootAbsPath []

lookupDef :: Name -> Context -> Maybe Expr
lookupDef v (Context _ defs) = lookup v defs

-----------------------------------------------------------------
