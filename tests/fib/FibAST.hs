module FibAST where

data Arith = Lit Int | Add Arith Arith | Sub Arith Arith | Fib Arith
             deriving Show