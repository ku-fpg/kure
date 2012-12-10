module Expr.Examples where

import Language.KURE

import Expr.AST
import Expr.Kure

-----------------------------------------------------------------

type RewriteE a     = Rewrite Context KureM a
type TranslateE a b = Translate Context KureM a b

-----------------------------------------------------------------

applyE :: TranslateE a b -> a -> Either String b
applyE t = runKureM Right Left . apply t initialContext

-----------------------------------------------------------------

inlineR :: RewriteE Expr
inlineR = withPatFailMsg "only variables can be inlined." $
          do (c, Var v) <- exposeT
             constT (lookupDef v c)

inlineGR :: RewriteE Generic
inlineGR = promoteR inlineR

-----------------------------------------------------------------

cmd1 :: Cmd
cmd1 = Seq (Assign "m" (Lit 7))
           (Assign "n" (Add (Lit 1) (Lit 2)))

expr1 :: Expr
expr1 = ESeq cmd1
             (Add (Var "m")
                  (Var "n")
             )

result1a :: Expr
result1a = ESeq cmd1
                (Add (Lit 7)
                     (Add (Lit 1) (Lit 2))
                )

result1b :: Expr
result1b = ESeq cmd1
                (Add (Lit 7)
                     (Var "n")
                )

test1a :: Bool
test1a = applyE (extractR (anytdR inlineGR)) expr1 == Right result1a

test1b :: Bool
test1b = applyE (extractR (onebuR inlineGR)) expr1 == Right result1b

test1c :: Bool
test1c = applyE (extractR (onetdR inlineGR)) expr1 == Right result1b

-----------------------------------------------------------------

expr2 :: Expr
expr2 = ESeq cmd1
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

-----------------------------------------------------------------

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

test3c :: Bool
test3c = applyE (extractR (alltdR inlineGR)) expr3 == Left "alltdR failed: only variables can be inlined."

-----------------------------------------------------------------

cmd4a :: Cmd
cmd4a = Assign "a" (Add (Lit 4) (Lit 5))

cmd4b :: Cmd
cmd4b = Assign "b" (Lit 6)

cmd4c :: Cmd
cmd4c = Assign "c" (Lit 7)

cmd4 :: Cmd
cmd4 = Seq cmd4a (Seq cmd4b cmd4c)

incrLitR :: RewriteE Expr
incrLitR = litT (Lit . succ)

incrLitGR :: RewriteE Generic
incrLitGR = promoteR incrLitR

isExpr :: TranslateE Generic Bool
isExpr = summandIsTypeT (undefined :: Expr)

result4a :: Cmd
result4a = Seq cmd4a
               (Seq (Assign "b" (Lit 7))
                    (Assign "c" (Lit 8))
               )

result4b :: Cmd
result4b = Seq cmd4a
               (Seq (Assign "b" (Lit 7))
                    cmd4c
               )

test4a :: Bool
test4a = applyE (extractR $ anyLargestR isExpr incrLitGR) cmd4 == Right result4a

test4b :: Bool
test4b = applyE (extractR $ oneLargestR isExpr incrLitGR) cmd4 == Right result4b

test4c :: Bool
test4c = applyE (extractR $ allLargestR isExpr incrLitGR) cmd4 == Left "allLargestR failed: allR failed: allR failed: not a Lit"

-----------------------------------------------------------------

checkTests :: Bool
checkTests = and [ test1a, test1b, test1c
                 , test2
                 , test3a, test3b, test3c
                 , test4a, test4b, test4c
                 ]

-----------------------------------------------------------------
