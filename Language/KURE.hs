-- |
-- Module: Language.KURE
-- Copyright: (c) 2006-2008 Andy Gill
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- This is the main import module for KURE, which exports all the major components.
--
--

module Language.KURE 
	( module Language.KURE.RewriteMonad
	, module Language.KURE.Translate
	, module Language.KURE.Rewrite
	, module Language.KURE.Combinators
	, module Language.KURE.Term
	) where

import Language.KURE.RewriteMonad
import Language.KURE.Translate
import Language.KURE.Rewrite
import Language.KURE.Combinators
import Language.KURE.Term
