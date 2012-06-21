{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}

module Fib.Kure where

import Language.KURE
import Fib.AST

import Control.Monad(guard)

--------------------------------------------------------------------------------------

-- | For this simple example, the context only contains the 'AbsolutePath', and 'Translate' always operates on 'Arith'.
type TranslateA b = Translate AbsolutePath Maybe Arith b
type RewriteA = TranslateA Arith

--------------------------------------------------------------------------------------

instance Term Arith where
  type Generic Arith = Arith

  numChildren (Lit _)   = 0
  numChildren (Add _ _) = 2
  numChildren (Sub _ _) = 2
  numChildren (Fib _)   = 1

instance Walker AbsolutePath Maybe Arith where

  childL n = lens $ translate $ \ c e ->
    do guard (hasChild n e)
       let c' = extendAbsPath n c
       case e of
         Add e1 e2  ->  case n of
                          0 -> return ((c',e1), \ e1' -> return (Add e1' e2))
                          1 -> return ((c',e2), \ e2' -> return (Add e1 e2'))
         Sub e1 e2  ->  case n of
                          0 -> return ((c',e1), \ e1' -> return (Sub e1' e2))
                          1 -> return ((c',e2), \ e2' -> return (Sub e1 e2'))
         Fib e1     ->  case n of
                          0 -> return ((c',e1), \ e1' -> return (Fib e1'))

--------------------------------------------------------------------------------------
