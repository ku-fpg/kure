{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}

module FibKure where

import Control.Applicative

import Language.KURE
import Fib

--------------------------------------------------------------------------------------

-- | For this simple example, the context is always empty and 'Translate' always operates on 'Arith'
type TranslateA b = Translate () Maybe Arith b
type RewriteA = TranslateA Arith

--------------------------------------------------------------------------------------

instance Term Arith where
  type Generic Arith = Arith

  numChildren (Lit _)   = 0
  numChildren (Add _ _) = 2
  numChildren (Sub _ _) = 2
  numChildren (Fib _)   = 1

instance Walker () Maybe Arith where

  childL n = lens $ \ c e -> case e of
                               Lit _      ->  empty
                               Add e1 e2  ->  case n of
                                                0 -> pure ((c,e1), \ e1' -> pure (Add e1' e2))
                                                1 -> pure ((c,e2), \ e2' -> pure (Add e1 e2'))
                                                _ -> empty
                               Sub e1 e2  ->  case n of
                                                0 -> pure ((c,e1), \ e1' -> pure (Sub e1' e2))
                                                1 -> pure ((c,e2), \ e2' -> pure (Sub e1 e2'))
                                                _ -> empty
                               Fib e1     ->  case n of
                                                0 -> pure ((c,e1), \ e1' -> pure (Fib e1'))
                                                _ -> empty

-- Using the default definitions of allR and anyR would be fine.
-- These are given here only to serve as an example.

  allR r = rewrite $ \ c ex -> case ex of
                                 Lit n      ->  pure (Lit n)
                                 Add e1 e2  ->  Add <$> apply r c e1 <*> apply r c e2
                                 Sub e1 e2  ->  Sub <$> apply r c e1 <*> apply r c e2
                                 Fib e      ->  Fib <$> apply r c e

  anyR r = rewrite $ \ c ex -> case ex of
                                 Lit _      ->  empty
                                 Add e1 e2  ->  do (b1,e1') <- apply (attemptR r) c e1
                                                   (b2,e2') <- apply (attemptR r) c e2
                                                   if b1 || b2
                                                    then return (Add e1' e2')
                                                    else empty
                                 Sub e1 e2  ->  do (b1,e1') <- apply (attemptR r) c e1
                                                   (b2,e2') <- apply (attemptR r) c e2
                                                   if b1 || b2
                                                    then return (Sub e1' e2')
                                                    else empty
                                 Fib e      ->  Fib <$> apply r c e

--------------------------------------------------------------------------------------
