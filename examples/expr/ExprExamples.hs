module ExprExamples where

import Control.Applicative

import Language.KURE
import Expr
import ExprKure

-----------------------------------------------------------------

inlineR :: RewriteE Expr
inlineR = rewrite $ \ c e -> case e of
                               Var v -> lookup v c
                               _     -> empty

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

result1 :: Expr
result1 = ESeq (Seq (Assign "m" (Lit 7))
                    (Assign "n" (Add (Lit 1) (Lit 2)))
               )
               (Add (Lit 7)
                    (Add (Lit 1) (Lit 2))
               )

test1 :: Bool
test1 = apply (extractR (anytdR inlineGR)) [] expr1 == Just result1

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
test2 = apply (extractR (anytdR inlineGR)) [] expr2 == Just result2

expr3 :: Expr
expr3 = ESeq (Assign "m" (Lit 7)
             )
             (Add (Var "y")
                  (Var "x")
             )

test3 :: Bool
test3 = apply (extractR (anytdR inlineGR)) [] expr3 == Nothing

-----------------------------------------------------------------

checkTests :: Bool
checkTests = and [ test1
                 , test2
                 , test3
                 ]

-----------------------------------------------------------------
