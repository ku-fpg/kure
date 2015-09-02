module Expr.Examples where

import Control.Arrow (arr)

import Data.Monoid (mempty)

import Language.KURE
import Language.KURE.Pathfinder

import Expr.AST
import Expr.Context
import Expr.Kure

-----------------------------------------------------------------

type RewriteE a     = Rewrite Context KureM a
type TransformE a b = Transform Context KureM a b

-----------------------------------------------------------------

applyE :: TransformE a b -> a -> Either String b
applyE t = runKureM Right (Left . showKureExc) . applyT t initialContext

-----------------------------------------------------------------

inlineR :: RewriteE Expr
inlineR = withPatFailExc (strategyFailure "inlineR") $
          do (c, Var v) <- exposeT
             constT (lookupDef v c)

inlineGR :: RewriteE Universe
inlineGR = promoteR inlineR

isAssign :: Universe -> Bool
isAssign (GCmd Assign{}) = True
isAssign _               = False

isESeq :: Universe -> Bool
isESeq (GExpr ESeq{}) = True
isESeq _              = False

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

result2a :: Expr
result2a = ESeq (Seq (Assign "m" (Lit 7))
                     (Assign "n" (Add (Lit 1) (Lit 2)))
                )
                (Add (Lit 7)
                     (Var "x")
                )

test2a :: Bool
test2a = applyE (extractR (anytdR inlineGR)) expr2 == Right result2a

----------------------------------------------------------------

assignMpath :: LocalPath Int
assignMpath = mempty @@ 0 @@ 0

assignNpath :: LocalPath Int
assignNpath = mempty @@ 0 @@ 1

test2b :: Bool
test2b = applyE (extractT $ pathsToT $ arr isAssign) expr2 == Right [assignMpath,assignNpath]

test2c :: Bool
test2c = applyE (extractT $ onePathToT $ arr isAssign) expr2 == Right assignMpath

test2d :: Bool
test2d = applyE (extractT $ oneNonEmptyPathToT $ arr isAssign) expr2 == Right assignMpath

test2e :: Bool
test2e = applyE (extractT $ onePathToT $ arr isESeq) expr2 == Right mempty

test2f :: Bool
test2f = applyE (extractT $ oneNonEmptyPathToT $ arr isESeq) expr2
    == Left "the oneNonEmptyPathToT strategy failed."

-----------------------------------------------------------------

expr3 :: Expr
expr3 = ESeq (Assign "m" (Lit 7)
             )
             (Add (Var "y")
                  (Var "x")
             )

test3a :: Bool
test3a = applyE (extractR (anytdR inlineGR)) expr3 == Left "the anytdR strategy failed."

test3b :: Bool
test3b = applyE (extractR (onetdR inlineGR)) expr3 == Left "the onetdR strategy failed."

test3c :: Bool
test3c = applyE (extractR (alltdR inlineGR)) expr3
    == Left "the alltdR strategy failed, because the inlineR strategy failed."

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

incrLitGR :: RewriteE Universe
incrLitGR = promoteR incrLitR

isExpr :: TransformE Universe Bool
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
test4c = applyE (extractR $ allLargestR isExpr incrLitGR) cmd4
    == Left "the allLargestR strategy failed, because the allR strategy failed, because the allR strategy failed, because the node was not a Lit."

-----------------------------------------------------------------

expr5 :: Expr
expr5 = Add (Var "n") (Lit 1)

test5a :: Bool
test5a = applyE (incrLitR <+> constT (throwM $ conditionalFailure "good")) expr5
    == Left "good"

test5b :: Bool
test5b = applyE (constT (throwM $ conditionalFailure "good") <+> idR) expr5
    == Left "good"

test5c :: Bool
test5c = applyE (constT (throwM $ conditionalFailure "good") <+ idR) expr5
    == Right expr5


-----------------------------------------------------------------

checkTests :: Bool
checkTests = and [ test1a, test1b, test1c
                 , test2a, test2b, test2c, test2d, test2e, test2f
                 , test3a, test3b, test3c
                 , test4a, test4b, test4c
                 , test5a, test5b, test5c
                 ]

-----------------------------------------------------------------
