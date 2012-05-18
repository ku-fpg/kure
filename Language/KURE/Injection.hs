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
-- and some useful interactions with 'Alternative'.

module Language.KURE.Injection where

import Control.Applicative

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

-------------------------------------------------------------------------------

-- | maps an effectful function over a retracted value, producing 'empty' if the retraction fails.
retractWith :: (Alternative m, Injection a a') => (a -> m b) -> a' -> m b
retractWith f = maybe empty f . retract

-- | maps a pure function over a retracted value, producing 'empty' if the retraction fails.
retractWithA :: (Alternative m, Injection a a') => (a -> b) -> a' -> m b
retractWithA f = retractWith (pure.f)

-- | retracts a value and lifts it into an 'Alternative', producing 'empty' if the retraction fails.
retractA :: (Alternative m, Injection a a') => a' -> m a
retractA = retractWithA id

-------------------------------------------------------------------------------
