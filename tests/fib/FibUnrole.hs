module FibUnrole where

import Control.Applicative

import Language.KURE
import FibAST
import FibKURE

type FibRewrite = Rewrite () Maybe Arith

applyFib :: FibRewrite -> Arith -> Maybe Arith
applyFib e = apply e ()

fib :: Int -> Arith
fib = Fib . Lit


fibDefR :: FibRewrite
fibDefR = rewrite $ \ () e -> case e of
                                   Fib (Lit 0)  -> pure (Lit 0)
                                   Fib (Lit 1)  -> pure (Lit 1)
                                   Fib (Lit n)  -> pure (Add (Fib (Lit (n-1))) 
                                                             (Fib (Lit (n-2)))
                                                        )
                                   _            -> empty
                                   
fibUnrollR :: FibRewrite
fibUnrollR = rewrite $ \ () e -> case e of
                                   Fib e  -> pure (Add (Fib (Sub e (Lit 1))) 
                                                       (Fib (Sub e (Lit 2)))
                                                  )
                                   _      -> empty                                   

addLitR :: FibRewrite
addLitR = rewrite $ \ () e -> case e of 
                                Add (Lit m) (Lit n) -> pure (Lit (m + n))
                                _                   -> empty

subLitR :: FibRewrite
subLitR = rewrite $ \ () e -> case e of 
                                Sub (Lit m) (Lit n) -> pure (Lit (m - n))
                                _                   -> empty


-- Just (Add (Fib (Sub (Lit 5) (Lit 1))) (Fib (Sub (Lit 5) (Lit 2))))
test1 :: Maybe Arith
test1 = applyFib fibUnrollR (fib 5)

-- Nothing
test2 :: Maybe Arith
test2 = applyFib addLitR (fib 5)

-- Nothing
test3 :: Maybe Arith
test3 = applyFib (fibUnrollR >-> addLitR) (fib 5)

-- Just (Add (Fib (Sub (Lit 5) (Lit 1))) (Fib (Sub (Lit 5) (Lit 2))))
test4 :: Maybe Arith
test4 = applyFib (fibUnrollR >-> bottomupR (tryR addLitR)) (fib 5)

-- Just (Add (Fib (Sub (Lit 5) (Lit 1))) (Fib (Sub (Lit 5) (Lit 2))))
test5 :: Maybe Arith
test5 = applyFib (fibUnrollR >-> bottomupR (tryR subLitR)) (fib 5)

-- Just (Lit 55)
test6 :: Maybe Arith
test6 = applyFib (tryR (topdownR fibDefR) >-> bottomupR (tryR addLitR)) (fib 10)

-- Just (Sub (Add (Lit 5) (Lit 3)) (Add (Fib (Sub (Lit 5) (Lit 1))) (Fib (Sub (Lit 5) (Lit 2)))))
test7 :: Maybe Arith
test7 = applyFib (anyR fibUnrollR) arith7

-- Just (Sub (Lit 8) (Fib (Lit 5)))
test8 :: Maybe Arith
test8 = applyFib (anyR addLitR) arith7

arith7 :: Arith
arith7 = Sub (Add (Lit 5) (Lit 3)) (fib 5)