module FibUnrole where

import Control.Applicative

import Language.KURE
import FibAST
import FibKURE

type FibRewrite = Rewrite () Maybe Arith

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
test1 = apply fibUnrollR () (fib 5)

-- Nothing
test2 :: Maybe Arith
test2 = apply addLitR () (fib 5)

-- Nothing
test3 :: Maybe Arith
test3 = apply (fibUnrollR >-> addLitR) () (fib 5)

-- Just (Add (Fib (Sub (Lit 5) (Lit 1))) (Fib (Sub (Lit 5) (Lit 2))))
test4 :: Maybe Arith
test4 = apply (fibUnrollR >-> bottomupR (tryR addLitR)) () (fib 5)

-- Just (Add (Fib (Sub (Lit 5) (Lit 1))) (Fib (Sub (Lit 5) (Lit 2))))
test5 :: Maybe Arith
test5 = apply (fibUnrollR >-> bottomupR (tryR subLitR)) () (fib 5)

-- Just (Lit 55)
test6 :: Maybe Arith
test6 = apply (tryR (topdownR fibDefR) >-> bottomupR (tryR addLitR)) () (fib 10)