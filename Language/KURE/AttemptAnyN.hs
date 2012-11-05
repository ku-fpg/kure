-- |
-- Module: Language.KURE.NthArg-Any
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

module Language.KURE.AttemptAnyN (AnyC, attemptAny, attemptAnyM) where

import Control.Monad (liftM, liftM2)

-- AnyC requires OverlappingInstances, but it can be redefined with
-- OverlappingTypeFamilyInstances in upcoming versions of GHC. That language
-- extension should actually enable a much simpler and more strongly-typed
-- definition of the whole AnyC apparatus.


{-# INLINE attemptAny #-}
{-# INLINE attemptAnyM #-}


attemptAny :: forall fun m z. (Monad m, AnyC fun m z) => fun -> z
attemptAny fun = anyC $ (return :: forall a. a -> m a) (fun, False)

attemptAnyM :: forall fun m z. (Monad m, AnyC fun m z) => m fun -> z
attemptAnyM fun = anyC $ (\f -> (f, False)) `liftM` fun



class AnyC fun m z | z -> m where anyC :: m (fun, Bool) -> z

-- fun is intended to be the type of a data constructor, so it is a series of
-- right nested ->s terminated by a data type

-- case 1: fun is a function
instance (Monad m, AnyC cod m z', z ~ (m (Bool, dom) -> z')) =>
  AnyC (dom -> cod) m z where
  {-# INLINE anyC #-}
  anyC m = \arg -> anyC $ liftM2 step m arg where
    {-# INLINE step #-}
    step = \(fun, b) (success, result) -> b `seq`
      (fun result, success || b)


-- case 2: fun is not a function
instance (Monad m, z ~ m not_a_fun) => AnyC not_a_fun m z where
  {-# INLINE anyC #-}
  anyC m = m >>= \(r, b) ->
    if b then return r else fail ("failed for all children")
