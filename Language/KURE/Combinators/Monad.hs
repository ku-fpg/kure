-- |
-- Module: Language.KURE.Combinators.Monad
-- Copyright: (c) 2012--2015 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides conditional monadic combinators.

module Language.KURE.Combinators.Monad
           ( -- * Monadic Conditionals
             guardExc
           , guardM
           , guardExcM
           , ifM
           , whenM
           , unlessM
           , guardMsg
) where

import Control.Monad (unless)
import Control.Monad.Catch (Exception, MonadThrow(..))

import Language.KURE.Exceptions

------------------------------------------------------------------------------------------

-- | Similar to 'guard', but invokes 'throwM' rather than 'mzero'.
guardExc :: (Exception e, MonadThrow m) => Bool -> e -> m ()
guardExc b e = unless b (throwM e)
{-# INLINE guardExc #-}

-- | As 'guardExc', but with a default exception.
guardM ::  MonadThrow m => Bool -> m ()
guardM b = guardExc b (strategyFailure "guardM")
{-# INLINE guardM #-}

-- | As 'guardExc', but with an @m Bool@ as argument.
guardExcM :: (Exception e, MonadThrow m) => m Bool -> e -> m ()
guardExcM mb e = do b <- mb
                    guardExc b e
{-# INLINE guardExcM #-}

-- | if-then-else lifted over a monadic predicate.
ifM ::  Monad m => m Bool -> m a -> m a -> m a
ifM mb m1 m2 = do b <- mb
                  if b then m1 else m2
{-# INLINE ifM #-}

-- | If the monadic predicate holds then perform the monadic action, else throw an exception.
whenM ::  MonadThrow m => m Bool -> m a -> m a
whenM mb ma = ifM mb ma (throwM $ strategyFailure "whenM")
{-# INLINE whenM #-}

-- | If the monadic predicate holds then throw an exception, else perform the monadic action.
unlessM ::  MonadThrow m => m Bool -> m a -> m a
unlessM mb ma = ifM mb (throwM $ strategyFailure "unlessM") ma
{-# INLINE unlessM #-}


guardMsg ::  (MonadFail m, Monad m) => Bool -> String -> m ()
guardMsg b msg = unless b (fail msg)


------------------------------------------------------------------------------------------
