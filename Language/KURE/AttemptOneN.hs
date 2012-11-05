-- |
-- Module: Language.KURE.NthArg-One
-- Copyright: (c) 2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Nicolas Frisby <nicolas.frisby@gmail.com>
-- Stability: beta
-- Portability: ghc
--
-- This module exports an extremely polymorphic function, 'nthArg', that is
-- useful in definitions of @childL@ in @Walker@ instances.
--
-- It's best to understand @nthArg@ by examining its usage; see the examples at
-- <TODO>.

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}

module Language.KURE.AttemptOneN (OneC, attemptOne, attemptOneM) where

import Language.KURE.Combinators (MonadCatch, (<<+))

import Control.Monad (liftM)

-- OneC requires OverlappingInstances, but it can be redefined with
-- OverlappingTypeFamilyInstances in upcoming versions of GHC. That language
-- extension should actually enable a much simpler and more strongly-typed
-- definition of the whole OneC apparatus.


{-# INLINE attemptOne #-}
{-# INLINE attemptOneM #-}


attemptOne :: forall fun m z. (Monad m, OneC fun m z) => fun -> z
attemptOne fun = oneC $ (return :: forall a. a -> m a) (P False fun)

attemptOneM :: forall fun m z. (Monad m, OneC fun m z) => m fun -> z
attemptOneM fun = oneC $ P False `liftM` fun




data P a b = P !a !b


class OneC fun m z | z -> m where oneC :: m (P Bool fun) -> z

-- fun is intended to be the type of a data constructor, so it is a series of
-- right nested ->s terminated by a data type

-- case 1: fun is a function
instance (MonadCatch m, OneC cod m z', z ~ (m (m dom, dom) -> z')) =>
  OneC (dom -> cod) m z where
  {-# INLINE oneC #-}
  oneC m = \arg -> oneC $ m >>= \(P b f) -> arg >>= \(ma, a) ->
    if b then return (P b $ f a) else
      (P True . f) `liftM` ma   <<+   return (P False $ f a)

-- case 2: fun is not a function
instance (Monad m, z ~ m not_a_fun) => OneC not_a_fun m z where
  {-# INLINE oneC #-}
  oneC m = m >>= \(P b z) -> if b then return z else fail "failed for all children"
