{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

-- |
-- Module: Language.KURE.ExtendableContext
-- Copyright: (c) 2012--2013 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides a utility data type for extending an existing context with extra information.
-- The idea is that, after defining class isntances for any user-specific contextual operations, it can be used for any ad-hoc context extensions.
-- See the treatment of 'ReadPath' and 'ExtendPath' as examples.

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

-- | Both components of the context are updated with the path crumbs.
instance (ExtendPath c crumb, ExtendPath e crumb) => ExtendPath (ExtendContext c e) crumb where
-- (@@) :: ExtendContext c e -> crumb -> ExtendContext c e
   (ExtendContext c e) @@ cr = ExtendContext (c @@ cr) (e @@ cr)

-- | The 'AbsolutePath' is read from the base context.
instance ReadPath c crumb => ReadPath (ExtendContext c e) crumb where
-- absPath :: ExtendContext c e -> AbsolutePath crumb
   absPath = absPath . baseContext

------------------------------------------------------------------------------------------------
