{-# LANGUAGE InstanceSigs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
-- |
-- Module: Language.KURE.Path
-- Copyright: (c) 2012--2013 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides several Path abstractions, used for denoting a path through the tree.

module Language.KURE.Path
       (
         -- * Paths
         -- | A @crumb@ is a value that denotes which child node to descended into.
         --   That is, a path through a tree is specified by a \"trail of breadcrumbs\".
         --   For example, if the children are numbered, 'Int' could be used as the @crumb@ type.
         --   'SnocPath' is useful for recording where you have been, as it is cheap to keep adding to the end of the list as you travel further.
         --   'Path' is useful for recording where you intend to go, as you'll need to access it in order.

         -- ** Relative Paths
         Path
         -- ** Snoc Paths
       , SnocPath(..)
       , ExtendPath(..)
       , snocPathToPath
       , pathToSnocPath
       , lastCrumb
         -- ** Absolute and Local Paths
       , LocalPath
       , AbsolutePath
       , ReadPath(..)
       , lastCrumbT
       , absPathT
       )
where

import Data.Monoid

import Control.Arrow ((>>^))

import Language.KURE.Translate
import Language.KURE.Combinators.Translate
import Language.KURE.Injection

-------------------------------------------------------------------------------

-- | A 'Path' is just a list.
--   The intent is that a path represents a route through the tree from an arbitrary node.
type Path crumb = [crumb]

-------------------------------------------------------------------------------

-- | A 'SnocPath' is a list stored in reverse order.
newtype SnocPath crumb = SnocPath [crumb] deriving Eq

instance Monoid (SnocPath crumb) where
   mempty :: SnocPath crumb
   mempty = SnocPath []

   mappend :: SnocPath crumb -> SnocPath crumb -> SnocPath crumb
   mappend (SnocPath p1) (SnocPath p2) = SnocPath (p2 ++ p1)

-- | Convert a 'Path' to a 'SnocPath'.  O(n).
pathToSnocPath :: Path crumb -> SnocPath crumb
pathToSnocPath p = SnocPath (reverse p)
{-# INLINE pathToSnocPath #-}

-- | Convert a 'SnocPath' to a 'Path'.  O(n).
snocPathToPath :: SnocPath crumb -> Path crumb
snocPathToPath (SnocPath p) = reverse p
{-# INLINE snocPathToPath #-}

instance Show crumb => Show (SnocPath crumb) where
   show :: SnocPath crumb -> String
   show = show . snocPathToPath
   {-# INLINE show #-}

-- | Get the last crumb from a 'SnocPath'.  O(1).
lastCrumb :: SnocPath crumb -> Maybe crumb
lastCrumb (SnocPath p) = safehead p
{-# INLINE lastCrumb #-}

-------------------------------------------------------------------------------

-- | A class of things that can be extended by crumbs.
--   Typically, @c@ is a context type.
--   The typical use is to extend an 'AbsolutePath' stored in the context (during tree traversal).
--   Note however, that if an 'AbsolutePath' is not stored in the context, an instance can still be declared with @(\@\@ cr)@ as an identity operation.
class ExtendPath c crumb | c -> crumb where
  -- | Extend the current 'AbsolutePath' by one crumb.
  (@@) :: c -> crumb -> c

-- | A 'SnocPath' from the root.
type AbsolutePath = SnocPath

-- | A 'SnocPath' from a local origin.
type LocalPath = SnocPath

-- | A class for contexts that store the current 'AbsolutePath', allowing transformations to depend upon it.
class ReadPath c crumb | c -> crumb where
  -- | Read the current absolute path.
  absPath :: c -> AbsolutePath crumb

-- | Lifted version of 'absPath'.
absPathT :: (ReadPath c crumb, Monad m) => Translate c m a (AbsolutePath crumb)
absPathT = contextT >>^ absPath
{-# INLINE absPathT #-}

-- | Lifted version of 'lastCrumb'.
lastCrumbT :: (ReadPath c crumb, Monad m) => Translate c m a crumb
lastCrumbT = contextonlyT (projectWithFailMsgM (fail "lastCrumbT failed: at the root, no crumbs yet.") . lastCrumb . absPath)
{-# INLINE lastCrumbT #-}

-------------------------------------------------------------------------------

-- | Any 'SnocPath' can be extended.
instance ExtendPath (SnocPath crumb) crumb where
   (@@) :: SnocPath crumb -> crumb -> SnocPath crumb
   (SnocPath crs) @@ cr = SnocPath (cr:crs)
   {-# INLINE (@@) #-}

-- | The simplest instance of 'ReadPath' is 'AbsolutePath' itself.
instance ReadPath (AbsolutePath crumb) crumb where
   absPath :: AbsolutePath crumb -> AbsolutePath crumb
   absPath = id
   {-# INLINE absPath #-}

-------------------------------------------------------------------------------

safehead :: [a] -> Maybe a
safehead []    = Nothing
safehead (a:_) = Just a
{-# INLINE safehead #-}

-------------------------------------------------------------------------------
