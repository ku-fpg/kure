-- |
-- Module: Language.KURE.Combinators.Monad
-- Copyright: (c) 2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides conditional monadic combinators.

module Language.KURE.Combinators.Monad
           ( -- * Monadic Conditionals
             guardMsg
           , guardM
           , ifM
           , whenM
           , unlessM
) where

import Control.Monad (unless)

------------------------------------------------------------------------------------------

-- | Similar to 'guard', but invokes 'fail' rather than 'mzero'.
guardMsg ::  Monad m => Bool -> String -> m ()
guardMsg b msg = unless b (fail msg)
{-# INLINE guardMsg #-}

-- | As 'guardMsg', but with a default error message.
guardM ::  Monad m => Bool -> m ()
guardM b = guardMsg b "guardM failed"
{-# INLINE guardM #-}

-- | if-then-else lifted over a monadic predicate.
ifM ::  Monad m => m Bool -> m a -> m a -> m a
ifM mb m1 m2 = do b <- mb
                  if b then m1 else m2
{-# INLINE ifM #-}

-- | If the monadic predicate holds then perform the monadic action, else fail.
whenM ::  Monad m => m Bool -> m a -> m a
whenM mb ma = ifM mb ma (fail "whenM: condition False")
{-# INLINE whenM #-}

-- | If the monadic predicate holds then fail, else perform the monadic action.
unlessM ::  Monad m => m Bool -> m a -> m a
unlessM mb ma = ifM mb (fail "unlessM: condition True") ma
{-# INLINE unlessM #-}

------------------------------------------------------------------------------------------
