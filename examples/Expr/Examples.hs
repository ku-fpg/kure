module Expr.Examples where

import Language.KURE
import Language.KURE.Injection

import Expr.AST
import Expr.Kure

-----------------------------------------------------------------

inlineR :: RewriteE Expr
inlineR = do (c, Var v) <- exposeT
             constT (lookupDef v c)

inlineGR :: RewriteE GenericExpr
inlineGR = promoteR inlineR

-----------------------------------------------------------------

expr1 :: Expr
expr1 = ESeq (Seq (Assign "m" (Lit 7))
                  (Assign "n" (Add (Lit 1) (Lit 2)))
             )
             (Add (Var "m")
                  (Var "n")
             )

result1a :: Expr
result1a = ESeq (Seq (Assign "m" (Lit 7))
                     (Assign "n" (Add (Lit 1) (Lit 2)))
                )
                (Add (Lit 7)
                     (Add (Lit 1) (Lit 2))
                )

result1b :: Expr
result1b = ESeq (Seq (Assign "m" (Lit 7))
                     (Assign "n" (Add (Lit 1) (Lit 2)))
                )
                (Add (Lit 7)
                     (Var "n")
                )

test1a :: Bool
test1a = applyE (extractR (anytdR inlineGR)) expr1 == Right result1a

test1b :: Bool
test1b = applyE (extractR (onebuR inlineGR)) expr1 == Right result1b

test1c :: Bool
test1c = applyE (extractR (onetdR inlineGR)) expr1 == Right result1b

expr2 :: Expr
expr2 = ESeq (Seq (Assign "m" (Lit 7))
                  (Assign "n" (Add (Lit 1) (Lit 2)))
             )
             (Add (Var "m")
                  (Var "x")
             )

result2 :: Expr
result2 = ESeq (Seq (Assign "m" (Lit 7))
                    (Assign "n" (Add (Lit 1) (Lit 2)))
               )
               (Add (Lit 7)
                    (Var "x")
               )

test2 :: Bool
test2 = applyE (extractR (anytdR inlineGR)) expr2 == Right result2

expr3 :: Expr
expr3 = ESeq (Assign "m" (Lit 7)
             )
             (Add (Var "y")
                  (Var "x")
             )

test3a :: Bool
test3a = applyE (extractR (anytdR inlineGR)) expr3 == Left "anytdR failed"

test3b :: Bool
test3b = applyE (extractR (onetdR inlineGR)) expr3 == Left "onetdR failed"

-----------------------------------------------------------------

checkTests :: Bool
checkTests = and [ test1a, test1b, test1c
                 , test2
                 , test3a, test3b
                 ]

-----------------------------------------------------------------
