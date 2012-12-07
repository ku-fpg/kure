{-# LANGUAGE MultiParamTypeClasses #-}

module Fib.Kure where

import Language.KURE
import Fib.AST

--------------------------------------------------------------------------------------

instance Walker AbsolutePath Arith where

  childrenL = multiLens $ translate $ \ c e ->
    let c0 = extendAbsPath 0 c
        c1 = extendAbsPath 1 c
     in case e of
          Lit n      ->  return ([],                \ []        -> return (Lit n))
          Add e1 e2  ->  return ([(c0,e1),(c1,e2)], \ [e1',e2'] -> return (Add e1' e2'))
          Sub e1 e2  ->  return ([(c0,e1),(c1,e2)], \ [e1',e2'] -> return (Sub e1' e2'))
          Fib e1     ->  return ([(c0,e1)],         \ [e1']     -> return (Fib e1'))

--------------------------------------------------------------------------------------
