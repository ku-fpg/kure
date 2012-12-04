{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}

module Fib.Kure where

import Language.KURE
import Language.KURE.Utilities(KureM,missingChild)
import Fib.AST

--------------------------------------------------------------------------------------

-- | For this simple example, the context is just an 'AbsolutePath', and 'Translate' always operates on 'Arith'.
type TranslateA b = Translate AbsolutePath KureM Arith b
type RewriteA = TranslateA Arith

--------------------------------------------------------------------------------------

instance Node Arith where

  numChildren (Lit _)   = 0
  numChildren (Add _ _) = 2
  numChildren (Sub _ _) = 2
  numChildren (Fib _)   = 1

instance Walker AbsolutePath KureM Arith where

  childL n = lens $ translate $ \ c e ->
    do guardMsg (hasChild n e) (missingChild n)
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
