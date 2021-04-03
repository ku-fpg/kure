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
       , projectWithFailMsgM
       -- * Transformation Injections
       , injectT
       , projectT
       , extractT
       , promoteT
       , projectWithFailMsgT
       , promoteWithFailMsgT
       -- * Rewrite Injections
       , extractR
       , promoteR
       , extractWithFailMsgR
       , promoteWithFailMsgR
) where

import Control.Arrow
import Control.Monad.Fail (MonadFail)

import Language.KURE.Transform

-------------------------------------------------------------------------------

-- | A class of injective functions from @a@ to @b@, and their projections.
--   The following law is expected to hold:
--
-- > project (inject a) == Just a

class Injection a u where
  inject  :: a -> u
  project :: u -> Maybe a

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

-- | As 'projectM', but takes a custom error message to use if projection fails.
projectWithFailMsgM :: (MonadFail m, Injection a u) => String -> u -> m a
projectWithFailMsgM msg = maybe (fail msg) return . project
{-# INLINE projectWithFailMsgM #-}

-- | Projects a value and lifts it into a 'Monad', with the possibility of failure.
projectM :: (MonadFail m, Injection a u) => u -> m a
projectM = projectWithFailMsgM "projectM failed"
{-# INLINE projectM #-}

-------------------------------------------------------------------------------

-- | Lifted 'inject'.
injectT :: (Monad m, Injection a u) => Transform c m a u
injectT = arr inject
{-# INLINE injectT #-}

projectWithFailMsgT :: (MonadFail m, Injection a u) => String -> Transform c m u a
projectWithFailMsgT = contextfreeT . projectWithFailMsgM
{-# INLINE projectWithFailMsgT #-}

-- | Lifted 'project', the transformation fails if the projection fails.
projectT :: (MonadFail m, Injection a u) => Transform c m u a
projectT = projectWithFailMsgT "projectT failed"
{-# INLINE projectT #-}

-- | Convert a transformation over an injected value into a transformation over a non-injected value.
extractT :: (Monad m, Injection a u) => Transform c m u b -> Transform c m a b
extractT t = injectT >>> t
{-# INLINE extractT #-}

-- | As 'promoteT', but takes a custom error message to use if promotion fails.
promoteWithFailMsgT  :: (MonadFail m, Injection a u) => String -> Transform c m a b -> Transform c m u b
promoteWithFailMsgT msg t = projectWithFailMsgT msg >>> t
{-# INLINE promoteWithFailMsgT #-}

-- | Promote a transformation over a value into a transformation over an injection of that value,
--   (failing if that injected value cannot be projected).
promoteT  :: (MonadFail m, Injection a u) => Transform c m a b -> Transform c m u b
promoteT = promoteWithFailMsgT "promoteT failed"
{-# INLINE promoteT #-}

-- | As 'extractR', but takes a custom error message to use if extraction fails.
extractWithFailMsgR :: (MonadFail m, Injection a u) => String -> Rewrite c m u -> Rewrite c m a
extractWithFailMsgR msg r = injectT >>> r >>> projectWithFailMsgT msg
{-# INLINE extractWithFailMsgR #-}

-- | Convert a rewrite over an injected value into a rewrite over a projection of that value,
--   (failing if that injected value cannot be projected).
extractR :: (MonadFail m, Injection a u) => Rewrite c m u -> Rewrite c m a
extractR = extractWithFailMsgR "extractR failed"
{-# INLINE extractR #-}

-- | As 'promoteR', but takes a custom error message to use if promotion fails.
promoteWithFailMsgR :: (MonadFail m, Injection a u) => String -> Rewrite c m a -> Rewrite c m u
promoteWithFailMsgR msg r = projectWithFailMsgT msg >>> r >>> injectT
{-# INLINE promoteWithFailMsgR #-}

-- | Promote a rewrite over a value into a rewrite over an injection of that value,
--   (failing if that injected value cannot be projected).
promoteR  :: (MonadFail m, Injection a u) => Rewrite c m a -> Rewrite c m u
promoteR = promoteWithFailMsgR "promoteR failed"
{-# INLINE promoteR #-}

-------------------------------------------------------------------------------
