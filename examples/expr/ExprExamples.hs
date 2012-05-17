module ExprExamples where

import Control.Applicative

import Language.KURE
import ExprLanguage
import ExprKURE

type ERewrite a = Rewrite Context Maybe a

inlineR :: ERewrite Expr
inlineR = rewrite $ \ c e -> case e of 
                               Var v -> lookup v c
                               _     -> empty

inlineGR :: ERewrite GenericExpr
inlineGR = promoteR inlineR

expr1 :: Expr
expr1 = ESeq (Seq (Assign "m" (Lit 7))
                  (Assign "n" (Add (Lit 1) (Lit 2)))
             ) 
             (Add (Var "m") 
                  (Var "n")
             )

-- Just (ESeq (Seq (Assign "m" (Lit 7)) 
--                 (Assign "n" (Add (Lit 1) (Lit 2)))
--            ) 
--            (Add (Lit 7) 
--                 (Add (Lit 1) (Lit 2)))
--            )
test1 :: Maybe Expr
test1 = apply (extractR (anytdR inlineGR)) [] expr1


expr2 :: Expr
expr2 = ESeq (Seq (Assign "m" (Lit 7))
                  (Assign "n" (Add (Lit 1) (Lit 2)))
             ) 
             (Add (Var "m") 
                  (Var "x")
             )
        
-- Just (ESeq (Seq (Assign "m" (Lit 7)) 
--                 (Assign "n" (Add (Lit 1) (Lit 2)))
--            ) 
--            (Add (Lit 7) 
--                 (Var "x"))
--            )
test2 :: Maybe Expr
test2 = apply (extractR (anytdR inlineGR)) [] expr2


expr3 :: Expr
expr3 = ESeq (Assign "m" (Lit 7)
             ) 
             (Add (Var "y") 
                  (Var "x")
             )
        
-- Nothing
test3 :: Maybe Expr
test3 = apply (extractR (anytdR inlineGR)) [] expr3

