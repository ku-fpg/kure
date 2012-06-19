{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}

module Fib.Kure where

import Control.Applicative

import Language.KURE
import Fib.AST

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

  childL n = lens $ translate $ \ c e -> let c' = extendAbsPath n c in
    case e of
      Lit _      ->  empty
      Add e1 e2  ->  case n of
                       0 -> pure ((c',e1), \ e1' -> pure (Add e1' e2))
                       1 -> pure ((c',e2), \ e2' -> pure (Add e1 e2'))
                       _ -> empty
      Sub e1 e2  ->  case n of
                       0 -> pure ((c',e1), \ e1' -> pure (Sub e1' e2))
                       1 -> pure ((c',e2), \ e2' -> pure (Sub e1 e2'))
                       _ -> empty
      Fib e1     ->  case n of
                       0 -> pure ((c',e1), \ e1' -> pure (Fib e1'))
                       _ -> empty

--------------------------------------------------------------------------------------
