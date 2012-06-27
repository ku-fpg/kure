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
       -- * Translate Injections
       , injectT
       , retractT
       , extractT
       , promoteT
       , extractR
       , promoteR
       -- * Lens Injections
       , injectL
       , retractL
) where

import Control.Monad
import Control.Arrow

import Language.KURE.Translate
import Language.KURE.Combinators

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
  inject  = id
  retract = Just

instance Injection a (Maybe a) where
  inject  = Just
  retract = id

-------------------------------------------------------------------------------

-- | Injects a value and lifts it into a 'Monad'.
injectM :: (Monad m, Injection a a') => a -> m a'
injectM = return . inject

-- | Retracts a value and lifts it into a 'MonadCatch', with the possibility of failure.
retractM :: (MonadCatch m, Injection a a') => a' -> m a
retractM = maybe (fail "retractM failed") return . retract

-------------------------------------------------------------------------------

-- | Lifted 'inject'.
injectT :: (Monad m, Injection a a') => Translate c m a a'
injectT = arr inject

-- | Lifted 'retract', the 'Translate' fails if the retraction fails.
retractT :: (MonadCatch m, Injection a a') => Translate c m a' a
retractT = contextfreeT retractM

-- | Convert a 'Translate' over an injected value into a 'Translate' over a non-injected value.
extractT :: (Monad m, Injection a a') => Translate c m a' b -> Translate c m a b
extractT t = injectT >>> t

-- | Promote a 'Translate' over a value into a 'Translate' over an injection of that value,
--   (failing if that injected value cannot be retracted).
promoteT  :: (MonadCatch m, Injection a a') => Translate c m a b -> Translate c m a' b
promoteT t = retractT >>> t

-- | Convert a 'Rewrite' over an injected value into a 'Rewrite' over a retraction of that value,
--   (failing if that injected value cannot be retracted).
extractR :: (MonadCatch m, Injection a a') => Rewrite c m a' -> Rewrite c m a
extractR r = injectT >>> r >>> retractT

-- | Promote a 'Rewrite' into over a value into a 'Rewrite' over an injection of that value,
--   (failing if that injected value cannot be retracted).
promoteR  :: (MonadCatch m, Injection a a') => Rewrite c m a -> Rewrite c m a'
promoteR r = retractT >>> r >>> injectT

-------------------------------------------------------------------------------

-- | A 'Lens' to the injection of a value.
injectL  :: (MonadCatch m, Injection a a') => Lens c m a a'
injectL = lens $ translate $ \ c a -> return ((c, inject a), retractM)

-- | A 'Lens' to the retraction of a value.
retractL :: (MonadCatch m, Injection a a') => Lens c m a' a
retractL = lens $ translate $ \ c -> retractM >=> (\ a -> return ((c,a), injectM))

-------------------------------------------------------------------------------
