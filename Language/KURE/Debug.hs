-- |
-- Module: Language.KURE.Combinators.Translate
-- Copyright: (c) 2012--2013 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides (unsafe) debugging/tracing combinators.
--
module Language.KURE.Debug (
        debugR
) where

import Prelude hiding (id, map, foldr, mapM)

import Debug.Trace

import Language.KURE.Combinators.Translate
import Language.KURE.Translate


-- | trace output of the value being rewritten; use for debugging only.
debugR :: (Monad m, Show a) => Int -> String -> Rewrite c m a
debugR n msg = acceptR (\ a -> trace (msg ++ " : " ++ take n (show a)) True)
