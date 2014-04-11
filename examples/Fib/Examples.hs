module Fib.Examples where

import Data.Monoid (mempty)

import Language.KURE

import Fib.AST
import Fib.Kure

-----------------------------------------------------------------------

-- | For this simple example, the context is just an 'AbsolutePath', and transformations always operates on 'Arith'.
type TransformA b = Transform (AbsolutePath Crumb) KureM Arith b
type RewriteA = TransformA Arith

-----------------------------------------------------------------------

applyFib :: TransformA b -> Arith -> Either String b
applyFib t = runKureM Right Left . applyT t mempty

-----------------------------------------------------------------------

-- | Apply the definition of the fibonacci function once.
--   Requires the argument to Fib to be a Literal.
fibLitR :: RewriteA
fibLitR = withPatFailMsg "fibLitR failed: not of form Fib (Lit n)" $
          do Fib (Lit n) <- idR
             case n of
               0  ->  return (Lit 0)
               1  ->  return (Lit 1)
               _  ->  return (Add (Fib (Sub (Lit n) (Lit 1)))
                                  (Fib (Sub (Lit n) (Lit 2)))
                             )

-- | Compute the addition of two literals.
addLitR :: RewriteA
addLitR = withPatFailMsg "addLitR failed" $
          do Add (Lit m) (Lit n) <- idR
             return (Lit (m + n))

-- | Compute the subtraction of two literals.
subLitR :: RewriteA
subLitR = withPatFailMsg "subLitR failed" $
          do Sub (Lit m) (Lit n) <- idR
             return (Lit (m - n))

-----------------------------------------------------------------------

arithR :: RewriteA
arithR = setFailMsg "arithR failed" $
         addLitR >+> subLitR

anyAddR :: RewriteA
anyAddR = anybuR addLitR

anySubR :: RewriteA
anySubR = anybuR subLitR

anyArithR :: RewriteA
anyArithR = anybuR arithR

allArithR :: RewriteA
allArithR = allbuR arithR

evalR :: RewriteA
evalR = innermostR (arithR >+> fibLitR)

-----------------------------------------------------------------------

expr1 :: Arith
expr1 = (3 + 7) - (4 + 1)

expr2 :: Arith
expr2 = Fib 8

expr3 :: Arith
expr3 = 100 - Fib (3 + 7)

test1a :: Bool
test1a = applyFib (allR addLitR) expr1
         ==
         Right (10 - 5)

test1b :: Bool
test1b = applyFib (alltdR addLitR) expr1
         ==
         Left "alltdR failed: addLitR failed"

test1c :: Bool
test1c = applyFib anyAddR expr1
         ==
         Right (10 - 5)

test1d :: Bool
test1d = applyFib anySubR expr1
         ==
         Left "anybuR failed"

test1e :: Bool
test1e = applyFib anyArithR expr1
         ==
         Right 5

test1f :: Bool
test1f = applyFib evalR expr1
         ==
         Right 5

test2a :: Bool
test2a = applyFib fibLitR expr2
         ==
         Right ((Fib (8 - 1)) + (Fib (8 - 2)))

test2b :: Bool
test2b = applyFib (anytdR fibLitR) expr2
         ==
         Right ((Fib (8 - 1)) + (Fib (8 - 2)))

test2c :: Bool
test2c = applyFib evalR expr2
         ==
         Right 21

test3a :: Bool
test3a = applyFib anyArithR expr3
         ==
         Right (100 - (Fib 10))

test3b :: Bool
test3b = applyFib (anyArithR >+> anyR fibLitR) expr3
         ==
         Right (100 - ((Fib (10 - 1)) + (Fib (10 - 2))))

test3c :: Bool
test3c = applyFib evalR expr3
         ==
         Right 45

test3d :: Bool
test3d = applyFib allArithR expr3
         ==
         Left "allbuR failed: allR failed: arithR failed"

-----------------------------------------------------------------------

checkTests :: Bool
checkTests = and [ test1a, test1b, test1c, test1d, test1e, test1f
                 , test2a, test2b, test2c
                 , test3a, test3b, test3c, test3d
                 ]

-----------------------------------------------------------------------
