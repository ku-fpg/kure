-- |
-- Module: Language.KURE.Debug
-- Copyright: (c) 2012--2014 The University of Kansas
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

import Debug.Trace

import Language.KURE.Combinators.Transform
import Language.KURE.Transform


-- | Trace output of the value being rewritten; use for debugging only.
debugR :: (Monad m, Show a) => Int -> String -> Rewrite c m a
debugR n msg = acceptR (\ a -> trace (msg ++ " : " ++ take n (show a)) True)
