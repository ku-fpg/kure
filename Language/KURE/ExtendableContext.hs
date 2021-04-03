{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module: Language.KURE.ExtendableContext
-- Copyright: (c) 2012--2021 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil.sculthorpe@ntu.ac.uk>
-- Stability: beta
-- Portability: ghc
--
-- This module provides a utility data type for extending an existing context with extra information.
-- The idea is that, after defining class instances for any user-specific contextual operations, it can be used for any ad-hoc context extensions.
-- See the treatment of 'ExtendPath' as an example.

module Language.KURE.ExtendableContext
        (
        -- * Extending Contexts
          ExtendContext
        , extendContext
        , baseContext
        , extraContext
) where

import Language.KURE.Path

------------------------------------------------------------------------------------------------

-- | A context transformer, for augmenting a context with additional information.
data ExtendContext c e = ExtendContext
                           { -- | Retrieve the base context (without the extra information).
                             baseContext   :: c
                             -- | Retrieve the extra contextual information.
                           , extraContext  :: e
                           }

-- | Extend a context with some additional information.
extendContext :: e -> c -> ExtendContext c e
extendContext e c = ExtendContext c e

-- | Both components of the context are updated with the crumb.
instance (ExtendPath c crumb, ExtendPath e crumb) => ExtendPath (ExtendContext c e) crumb where
   (@@) :: ExtendContext c e -> crumb -> ExtendContext c e
   (ExtendContext c e) @@ cr = ExtendContext (c @@ cr) (e @@ cr)

------------------------------------------------------------------------------------------------
