{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module: Language.KURE.Injection
-- Copyright: (c) 2006-2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: alpha
-- Portability: ghc
--
-- This module provides a type class for injective functions (and their retractions),
-- and some useful interactions with 'Translate'.
-- A particularly useful instance of Injection is from @a@ to 'Generic' @a@,
-- and that case is the primary purpose of most of these combinators.

module Language.KURE.Injection where

import Control.Monad
import Control.Arrow

import Language.KURE.Translate

-------------------------------------------------------------------------------

-- | A class of injective functions from @a@ to @b@, and their retractions.
--   The following law is expected to hold:  retract (inject a) == Just a
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

-- | injects a value and lifts it into a 'Monad'.
injectM :: (Monad m, Injection a a') => a -> m a'
injectM = return . inject

-- | retracts a value and lifts it into a 'MonadPlus', producing 'mzero' if the retraction fails.
retractM :: (MonadPlus m, Injection a a') => a' -> m a
retractM = maybe mzero return . retract

-------------------------------------------------------------------------------

-- | lifted 'inject'.
injectT :: (Monad m, Injection a a') => Translate c m a a'
injectT = arr inject

-- | lifted 'retract', the 'Translate' fails if the retraction fails.
retractT :: (MonadPlus m, Injection a a') => Translate c m a' a
retractT = contextfreeT retractM

-- | 'extractT' converts a 'Translate' over an injected value into a 'Translate' over a non-injected value.
extractT :: (Monad m, Injection a a') => Translate c m a' b -> Translate c m a b
extractT t = injectT >>> t

-- | 'promoteT' promotes a 'Translate' over a value into a 'Translate' over an injection of that value,
--   (failing if that injected value cannot be retracted).
promoteT  :: (MonadPlus m, Injection a a') => Translate c m a b -> Translate c m a' b
promoteT t = retractT >>> t

-- | 'extractR' converts a 'Rewrite' over an injected value into a 'Rewrite' over a retraction of that value,
--   (failing if that injected value cannot be retracted).
extractR :: (MonadPlus m, Injection a a') => Rewrite c m a' -> Rewrite c m a
extractR r = injectT >>> r >>> retractT

-- | 'promoteR' promotes a 'Rewrite' into over a value into a 'Rewrite' over an injection of that value,
--   (failing if that injected value cannot be retracted).
promoteR  :: (MonadPlus m, Injection a a') => Rewrite c m a -> Rewrite c m a'
promoteR r = retractT >>> r >>> injectT

-------------------------------------------------------------------------------

-- | a 'Lens' to the 'Injection' of a value.
injectL  :: (MonadPlus m, Injection a a') => Lens c m a a'
injectL = lens $ \ c a -> return ((c, inject a), retractM)

-- | a 'Lens' to the retraction of a value.
retractL :: (MonadPlus m, Injection a a') => Lens c m a' a
retractL = lens $ \ c -> retractM >=> (\ a -> return ((c,a), injectM))

-------------------------------------------------------------------------------
