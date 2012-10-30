-- |
-- Module: Language.KURE.NthArg
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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GADTs #-}

module Language.KURE.NthArg (Nat1(..), nthArg) where

import Language.KURE.Combinators (MonadCatch)
import Language.KURE.Walker (Generic, Node)

import Language.KURE.Utilities (childLaux)

data Nat = TyZ | TyS Nat

data Nat1 :: Nat -> * where
  Z :: Nat1 TyZ
  S :: Nat1 n -> Nat1 (TyS n)

data Proxy (t :: Nat) = Proxy

-- specializing the C/C2 apparatus to our intended use case
--
-- the type refinement in the C constraint eventually determines the
-- inaccessible type variables c, x, dt, and m
nthArg :: forall n fun c x dt m z.
           (MonadCatch m, Node x,
            C n fun c x dt ((c, Generic x), Generic x -> m dt) z) =>
           Nat1 n -> fun -> z
nthArg _ =
  method (Proxy :: Proxy n)
           (childLaux :: ((c, x) -> (x -> dt) -> ((c, Generic x), Generic x -> m dt)))



-- C/method handles the args up to and including the one distinguished by n

-- C2/method2 adds on the args after the one distinguished by n

-- C2 requires OverlappingInstances, but it can be redefined with
-- OverlappingTypeFamilyInstances in upcoming versions of GHC. That language
-- extension should actually enable a much simpler and more strongly-typed
-- definition of the whole C/C2 apparatus.



-- x  is the nth argument of fun
-- dt is the final codomain of fun
-- r is the result of the continuation
-- z is the type of method after three applications
--
-- n, fun, c, and r should be considered as inputs
class C (n :: Nat) (fun :: *) c x dt r z where
  method :: Proxy n -> ((c, x) -> (x -> dt) -> r) -> fun -> z

instance (x ~ dom, C2 cod dt r z', z ~ ((c, x) -> z')) =>
  C TyZ (dom -> cod) c x dt r z where
  method _ k fun = \y -> method2 (k y) fun

instance (C n cod c x dt r z', z ~ (dom -> z')) =>
  C (TyS n) (dom -> cod) c x dt r z where
  method _ k fun = \x -> method (Proxy :: Proxy n) k (fun x)



-- called by the C TyZ instance
class C2 (fun :: *) dt r z where
  method2 :: ((x -> dt) -> r) -> (x -> fun) -> z

-- fun is intended to be the type of a data constructor, so it is a series of
-- right nested ->s terminated by a data type

-- case 1: fun is a function
instance (C2 cod dt r z', z ~ (dom -> z')) =>
  C2 (dom -> cod) dt r z where
  method2 k fun = \x -> method2 k (($ x) . fun)

-- case 2: fun is not a function
instance (not_a_fun ~ dt, r ~ z) => C2 not_a_fun dt r z where
  method2 k fun = k fun
