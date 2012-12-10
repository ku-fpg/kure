{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module: Language.KURE.Injection
-- Copyright: (c) 2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides a type class for injective functions (and their retractions),
-- and some useful interactions with 'Translate'.
--
-- A particularly useful instance of 'Injection' is from @a@ to 'Generic' @a@,
-- and that case is the primary purpose of most of these combinators.

module Language.KURE.Injection
       ( -- * Injection Class
         Injection(..)
       -- * Monad Injections
       , injectM
       , retractM
       , retractWithFailMsgM
       -- * Translate Injections
       , injectT
       , retractT
       , extractT
       , promoteT
       , promoteWithFailMsgT
       -- * Rewrite Injections
       , extractR
       , promoteR
       , extractWithFailMsgR
       , promoteWithFailMsgR
) where

import Control.Arrow

import Language.KURE.Translate

-------------------------------------------------------------------------------

-- | A class of injective functions from @a@ to @b@, and their retractions.
--   The following law is expected to hold:
--
-- > retract (inject a) == Just a

class Injection a b where
  inject  :: a -> b
  retract :: b -> Maybe a

-- | There is an identity injection for all types.
instance Injection a a where
  {-# INLINE inject #-}
  inject  = id
  {-# INLINE retract #-}
  retract = Just

instance Injection a (Maybe a) where
  {-# INLINE inject #-}
  inject  = Just
  {-# INLINE retract #-}
  retract = id

-------------------------------------------------------------------------------

-- | Injects a value and lifts it into a 'Monad'.
injectM :: (Monad m, Injection a g) => a -> m g
injectM = return . inject
{-# INLINE injectM #-}

-- | As 'retractM', but takes a custom error message to use if retraction fails.
retractWithFailMsgM :: (Monad m, Injection a g) => String -> g -> m a
retractWithFailMsgM msg = maybe (fail msg) return . retract
{-# INLINE retractWithFailMsgM #-}

-- | Retracts a value and lifts it into a 'MonadCatch', with the possibility of failure.
retractM :: (Monad m, Injection a g) => g -> m a
retractM = retractWithFailMsgM "retractM failed"
{-# INLINE retractM #-}

-------------------------------------------------------------------------------

-- | Lifted 'inject'.
injectT :: (Monad m, Injection a g) => Translate c m a g
injectT = arr inject
{-# INLINE injectT #-}

retractWithFailMsgT :: (Monad m, Injection a g) => String -> Translate c m g a
retractWithFailMsgT = contextfreeT . retractWithFailMsgM
{-# INLINE retractWithFailMsgT #-}

-- | Lifted 'retract', the 'Translate' fails if the retraction fails.
retractT :: (Monad m, Injection a g) => Translate c m g a
retractT = retractWithFailMsgT "retractT failed"
{-# INLINE retractT #-}

-- | Convert a 'Translate' over an injected value into a 'Translate' over a non-injected value.
extractT :: (Monad m, Injection a g) => Translate c m g b -> Translate c m a b
extractT t = injectT >>> t
{-# INLINE extractT #-}

-- | As 'promoteT', but takes a custom error message to use if promotion fails.
promoteWithFailMsgT  :: (Monad m, Injection a g) => String -> Translate c m a b -> Translate c m g b
promoteWithFailMsgT msg t = retractWithFailMsgT msg >>> t
{-# INLINE promoteWithFailMsgT #-}

-- | Promote a 'Translate' over a value into a 'Translate' over an injection of that value,
--   (failing if that injected value cannot be retracted).
promoteT  :: (Monad m, Injection a g) => Translate c m a b -> Translate c m g b
promoteT = promoteWithFailMsgT "promoteT failed"
{-# INLINE promoteT #-}

-- | As 'extractR', but takes a custom error message to use if extraction fails.
extractWithFailMsgR :: (Monad m, Injection a g) => String -> Rewrite c m g -> Rewrite c m a
extractWithFailMsgR msg r = injectT >>> r >>> retractWithFailMsgT msg
{-# INLINE extractWithFailMsgR #-}

-- | Convert a 'Rewrite' over an injected value into a 'Rewrite' over a retraction of that value,
--   (failing if that injected value cannot be retracted).
extractR :: (Monad m, Injection a g) => Rewrite c m g -> Rewrite c m a
extractR = extractWithFailMsgR "extractR failed"
{-# INLINE extractR #-}

-- | As 'promoteR', but takes a custom error message to use if promotion fails.
promoteWithFailMsgR :: (Monad m, Injection a g) => String -> Rewrite c m a -> Rewrite c m g
promoteWithFailMsgR msg r = retractWithFailMsgT msg >>> r >>> injectT
{-# INLINE promoteWithFailMsgR #-}

-- | Promote a 'Rewrite' into over a value into a 'Rewrite' over an injection of that value,
--   (failing if that injected value cannot be retracted).
promoteR  :: (Monad m, Injection a g) => Rewrite c m a -> Rewrite c m g
promoteR = promoteWithFailMsgR "promoteR failed"
{-# INLINE promoteR #-}

-------------------------------------------------------------------------------
