-- |
-- Module: Language.KURE.Rewrite 
-- Copyright: (c) 2006-2008 Andy Gill
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc
--

module Language.KURE.Rewrite where

import Language.KURE.RewriteMonad
import Language.KURE.Translate
import Data.Monoid

type Rewrite m dec exp = Translate m dec exp exp

rebuild :: (Monoid dec, Monad m) => (dec -> exp1 -> RewriteM m dec exp1) -> Rewrite m dec exp1
rebuild = translateWithId

rewrite :: (Monoid dec, Monad m) => (dec -> exp1 -> RewriteM m dec exp1) -> Rewrite m dec exp1
rewrite = translate

idR :: (Monad m, Monoid dec) => Rewrite m dec exp
idR = rebuild $ \ dec e -> return e

failR :: (Monad m) => String -> Rewrite m dec a
failR = failT

