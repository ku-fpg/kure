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
-- This module provides a type class for injective functions (and their projections),
-- and some useful interactions with 'Translate'.
--
module Language.KURE.Injection
       ( -- * Injection Class
         Injection(..)
       -- * Monad Injections
       , injectM
       , projectM
       , projectWithFailMsgM
       -- * Translate Injections
       , injectT
       , projectT
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

-- | A class of injective functions from @a@ to @b@, and their projections.
--   The following law is expected to hold:
--
-- > project (inject a) == Just a

class Injection a b where
  inject  :: a -> b
  project :: b -> Maybe a

-- | There is an identity injection for all types.
instance Injection a a where
  {-# INLINE inject #-}
  inject  = id
  {-# INLINE project #-}
  project = Just

instance Injection a (Maybe a) where
  {-# INLINE inject #-}
  inject  = Just
  {-# INLINE project #-}
  project = id

-------------------------------------------------------------------------------

-- | Injects a value and lifts it into a 'Monad'.
injectM :: (Monad m, Injection a g) => a -> m g
injectM = return . inject
{-# INLINE injectM #-}

-- | As 'projectM', but takes a custom error message to use if projection fails.
projectWithFailMsgM :: (Monad m, Injection a g) => String -> g -> m a
projectWithFailMsgM msg = maybe (fail msg) return . project
{-# INLINE projectWithFailMsgM #-}

-- | Projects a value and lifts it into a 'MonadCatch', with the possibility of failure.
projectM :: (Monad m, Injection a g) => g -> m a
projectM = projectWithFailMsgM "projectM failed"
{-# INLINE projectM #-}

-------------------------------------------------------------------------------

-- | Lifted 'inject'.
injectT :: (Monad m, Injection a g) => Translate c m a g
injectT = arr inject
{-# INLINE injectT #-}

projectWithFailMsgT :: (Monad m, Injection a g) => String -> Translate c m g a
projectWithFailMsgT = contextfreeT . projectWithFailMsgM
{-# INLINE projectWithFailMsgT #-}

-- | Lifted 'project', the 'Translate' fails if the projection fails.
projectT :: (Monad m, Injection a g) => Translate c m g a
projectT = projectWithFailMsgT "projectT failed"
{-# INLINE projectT #-}

-- | Convert a 'Translate' over an injected value into a 'Translate' over a non-injected value.
extractT :: (Monad m, Injection a g) => Translate c m g b -> Translate c m a b
extractT t = injectT >>> t
{-# INLINE extractT #-}

-- | As 'promoteT', but takes a custom error message to use if promotion fails.
promoteWithFailMsgT  :: (Monad m, Injection a g) => String -> Translate c m a b -> Translate c m g b
promoteWithFailMsgT msg t = projectWithFailMsgT msg >>> t
{-# INLINE promoteWithFailMsgT #-}

-- | Promote a 'Translate' over a value into a 'Translate' over an injection of that value,
--   (failing if that injected value cannot be projected).
promoteT  :: (Monad m, Injection a g) => Translate c m a b -> Translate c m g b
promoteT = promoteWithFailMsgT "promoteT failed"
{-# INLINE promoteT #-}

-- | As 'extractR', but takes a custom error message to use if extraction fails.
extractWithFailMsgR :: (Monad m, Injection a g) => String -> Rewrite c m g -> Rewrite c m a
extractWithFailMsgR msg r = injectT >>> r >>> projectWithFailMsgT msg
{-# INLINE extractWithFailMsgR #-}

-- | Convert a 'Rewrite' over an injected value into a 'Rewrite' over a projection of that value,
--   (failing if that injected value cannot be projected).
extractR :: (Monad m, Injection a g) => Rewrite c m g -> Rewrite c m a
extractR = extractWithFailMsgR "extractR failed"
{-# INLINE extractR #-}

-- | As 'promoteR', but takes a custom error message to use if promotion fails.
promoteWithFailMsgR :: (Monad m, Injection a g) => String -> Rewrite c m a -> Rewrite c m g
promoteWithFailMsgR msg r = projectWithFailMsgT msg >>> r >>> injectT
{-# INLINE promoteWithFailMsgR #-}

-- | Promote a 'Rewrite' into over a value into a 'Rewrite' over an injection of that value,
--   (failing if that injected value cannot be projected).
promoteR  :: (Monad m, Injection a g) => Rewrite c m a -> Rewrite c m g
promoteR = promoteWithFailMsgR "promoteR failed"
{-# INLINE promoteR #-}

-------------------------------------------------------------------------------
