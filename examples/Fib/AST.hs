module Fib.AST (Arith (..)) where

data Arith = Lit Int | Add Arith Arith | Sub Arith Arith | Fib Arith deriving Eq

instance Show Arith where
  show (Lit n)   = show n
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Fib x)   = "(Fib " ++ show x ++ ")"

instance Num Arith where
  (+)         = Add
  (-)         = Sub
  fromInteger = Lit . fromInteger
  negate x    = Sub 0 x
  (*)         = error "Multiplication not defined for Arith"
  abs         = error "Absolute value not defined for Arith"
  signum      = error "Signum not defined for Arith"
