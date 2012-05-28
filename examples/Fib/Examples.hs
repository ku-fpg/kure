module Fib.Examples where

import Language.KURE

import Fib.AST
import Fib.Kure

-----------------------------------------------------------------------

applyFib :: RewriteA -> Arith -> Maybe Arith
applyFib e = apply e ()

-----------------------------------------------------------------------

-- | Apply the definition of the fibonacci function once.
--   Requires the argument to Fib to be a Literal.
fibLitR :: RewriteA
fibLitR = do Fib (Lit n) <- idR
             case n of
               0  ->  return (Lit 0)
               1  ->  return (Lit 1)
               _  ->  return (Add (Fib (Sub (Lit n) (Lit 1)))
                                  (Fib (Sub (Lit n) (Lit 2)))
                             )

-- | Compute the addition of two literals.
addLitR :: RewriteA
addLitR = do Add (Lit m) (Lit n) <- idR
             return (Lit (m + n))

-- | Compute the subtraction of two literals.
subLitR :: RewriteA
subLitR = do Sub (Lit m) (Lit n) <- idR
             return (Lit (m - n))

-----------------------------------------------------------------------

arithR :: RewriteA
arithR = addLitR >+> subLitR

anyAddR :: RewriteA
anyAddR = anybuR addLitR

anySubR :: RewriteA
anySubR = anybuR subLitR

anyArithR :: RewriteA
anyArithR = anybuR arithR

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
test1a = applyFib anyAddR expr1
         ==
         Just (10 - 5)

test1b :: Bool
test1b = applyFib anySubR expr1
         ==
         Nothing

test1c :: Bool
test1c = applyFib anyArithR expr1
         ==
         Just 5

test1d :: Bool
test1d = applyFib evalR expr1
         ==
         Just 5

test2a :: Bool
test2a = applyFib fibLitR expr2
         ==
         Just ((Fib (8 - 1)) + (Fib (8 - 2)))

test2b :: Bool
test2b = applyFib (anytdR fibLitR) expr2
         ==
         Just ((Fib (8 - 1)) + (Fib (8 - 2)))

test2c :: Bool
test2c = applyFib evalR expr2
         ==
         Just 21

test3a :: Bool
test3a = applyFib anyArithR expr3
         ==
         Just (100 - (Fib 10))

test3b :: Bool
test3b = applyFib (anyArithR >+> anyR fibLitR) expr3
         ==
         Just (100 - ((Fib (10 - 1)) + (Fib (10 - 2))))

test3c :: Bool
test3c = applyFib evalR expr3
         ==
         Just 45

-----------------------------------------------------------------------

checkTests :: Bool
checkTests = and [ test1a, test1b, test1c, test1d
                 , test2a, test2b, test2c, test3a
                 , test3b, test3c
                 ]

-----------------------------------------------------------------------
