{-# LANGUAGE Rank2Types #-}

-- |
-- Module: Language.KURE.Rewrite 
-- Copyright: (c) 2006-2008 Andy Gill
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- 'Rewrite' is a synonoym for a 'Translate' with the same source and target type.
-- This module contains the defintion of Rewrite, and some aliases for some translate functions that use 
-- Rewrite rather than Translate.

module Language.KURE.Rewrite 
	( Rewrite
	, rewrite
	) where

import Language.KURE.RewriteMonad
import Language.KURE.Translate
import Data.Monoid

-- | A 'Rewrite' is a 'Translate' that shares the same source and target type. Literally, 
-- a 'Rewrite' provides the details about how to /re-write/ a specific type.

type Rewrite exp = Translate exp exp

-- | 'rewrite' is our primitive way of building a Rewrite,
--  where if the rewrite is successful it is automatically marked as a non-identity rewrite. 
--
-- @rewrite $ \\ _ e -> return e@ /is not/ an identity rewrite. 

rewrite :: (forall m . (TranslateMonad m) => exp1 -> m exp1) -> Rewrite exp1
rewrite = translate

