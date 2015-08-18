{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
-- |
-- Module: Language.KURE.Injection
-- Copyright: (c) 2012--2014 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides a type class for injective functions (and their projections),
-- and some useful interactions with 'Transform'.
--
module Language.KURE.Injection
       ( -- * Injection Class
         Injection(..)
       -- * Monad Injections
       , injectM
       , projectM
       , projectWithFailExcM
       -- * Transformation Injections
       , injectT
       , projectT
       , extractT
       , promoteT
       , projectWithFailExcT
       , promoteWithFailExcT
       -- * Rewrite Injections
       , extractR
       , promoteR
       , extractWithFailExcR
       , promoteWithFailExcR
) where

import Control.Arrow
import Control.Monad.Catch

#if __GLASGOW_HASKELL__ >= 708
import Data.Typeable
#endif

import Language.KURE.Exceptions
import Language.KURE.Transform

-------------------------------------------------------------------------------

-- | A class of injective functions from @a@ to @b@, and their projections.
--   The following law is expected to hold:
--
-- > project (inject a) == Just a

class Injection a u where
  inject  :: a -> u
  project :: u -> Maybe a

#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable Injection
#endif

-- | There is an identity injection for all types.
instance Injection a a where
  inject :: a -> a
  inject = id
  {-# INLINE inject #-}

  project :: a -> Maybe a
  project = Just
  {-# INLINE project #-}

instance Injection a (Maybe a) where
  inject :: a -> Maybe a
  inject  = Just
  {-# INLINE inject #-}

  project :: Maybe a -> Maybe a
  project = id
  {-# INLINE project #-}

-------------------------------------------------------------------------------

-- | Injects a value and lifts it into a 'Monad'.
injectM :: (Monad m, Injection a u) => a -> m u
injectM = return . inject
{-# INLINE injectM #-}

-- | As 'projectM', but takes a custom exception to use if projection fails.
projectWithFailExcM :: (Exception e, MonadThrow m, Injection a u) => e -> u -> m a
projectWithFailExcM e = maybe (throwM e) return . project
{-# INLINE projectWithFailExcM #-}

-- | Projects a value and lifts it into a 'Monad', with the possibility of failure.
projectM :: (MonadThrow m, Injection a u) => u -> m a
projectM = projectWithFailExcM $ strategyFailure "projectM"
{-# INLINE projectM #-}

-------------------------------------------------------------------------------

-- | Lifted 'inject'.
injectT :: (Monad m, Injection a u) => Transform c m a u
injectT = arr inject
{-# INLINE injectT #-}

projectWithFailExcT :: (Exception e, MonadThrow m, Injection a u) => e -> Transform c m u a
projectWithFailExcT = contextfreeT . projectWithFailExcM
{-# INLINE projectWithFailExcT #-}

-- | Lifted 'project', the transformation fails if the projection fails.
projectT :: (MonadThrow m, Injection a u) => Transform c m u a
projectT = projectWithFailExcT $ strategyFailure "projectT"
{-# INLINE projectT #-}

-- | Convert a transformation over an injected value into a transformation over a non-injected value.
extractT :: (Monad m, Injection a u) => Transform c m u b -> Transform c m a b
extractT t = injectT >>> t
{-# INLINE extractT #-}

-- | As 'promoteT', but takes a custom exception to use if promotion fails.
promoteWithFailExcT  :: (Exception e, MonadThrow m, Injection a u) => e -> Transform c m a b -> Transform c m u b
promoteWithFailExcT e t = projectWithFailExcT e >>> t
{-# INLINE promoteWithFailExcT #-}

-- | Promote a transformation over a value into a transformation over an injection of that value,
--   (throwing an exception if that injected value cannot be projected).
promoteT  :: (MonadThrow m, Injection a u) => Transform c m a b -> Transform c m u b
promoteT = promoteWithFailExcT $ strategyFailure "promoteT"
{-# INLINE promoteT #-}

-- | As 'extractR', but takes a custom exception to use if extraction fails.
extractWithFailExcR :: (Exception e, MonadThrow m, Injection a u) => e -> Rewrite c m u -> Rewrite c m a
extractWithFailExcR e r = injectT >>> r >>> projectWithFailExcT e
{-# INLINE extractWithFailExcR #-}

-- | Convert a rewrite over an injected value into a rewrite over a projection of that value,
--   (throwing an exception if that injected value cannot be projected).
extractR :: (MonadThrow m, Injection a u) => Rewrite c m u -> Rewrite c m a
extractR = extractWithFailExcR $ strategyFailure "extractR"
{-# INLINE extractR #-}

-- | As 'promoteR', but takes a custom exception to use if promotion fails.
promoteWithFailExcR :: (Exception e, MonadThrow m, Injection a u) => e -> Rewrite c m a -> Rewrite c m u
promoteWithFailExcR e r = projectWithFailExcT e >>> r >>> injectT
{-# INLINE promoteWithFailExcR #-}

-- | Promote a rewrite over a value into a rewrite over an injection of that value,
--   (throwing an exception if that injected value cannot be projected).
promoteR  :: (MonadThrow m, Injection a u) => Rewrite c m a -> Rewrite c m u
promoteR = promoteWithFailExcR $ strategyFailure "promoteR"
{-# INLINE promoteR #-}

-------------------------------------------------------------------------------
