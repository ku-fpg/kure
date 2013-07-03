{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

-- |
-- Module: Language.KURE.Pathfinder
-- Copyright: (c) 2012--2013 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides combinators to find paths sub-nodes specified by a predicate.
-- They live in their own module as they are mostly an unprincipled assortment of combinators,
-- characterised by having been useful in practice.
-- I think some key abstractions still need to be identified, and the whole set of combinators restructured.

module Language.KURE.Pathfinder
        (
        -- * Translations for Finding LocalPaths
          pathsToT
        , onePathToT
        , oneNonEmptyPathToT
        , prunePathsToT
        , uniquePathToT
        , uniquePrunePathToT
) where

import Control.Category hiding ((.))
import Data.Monoid (mempty)

import Language.KURE.MonadCatch
import Language.KURE.Translate
import Language.KURE.Combinators.Translate
import Language.KURE.Path
import Language.KURE.Walker
import Language.KURE.ExtendableContext

-------------------------------------------------------------------------------

-- Apply a local Translate, using only a local path as context.
applySnocPathT :: Translate (SnocPath crumb) m a b -> Translate c m a b
applySnocPathT = liftContext (\ _ -> mempty)


-- | Find the 'Path's to every node that satisfies the predicate.
pathsToT :: forall c crumb g m. (Walker (SnocPath crumb) g, MonadCatch m) => (g -> Bool) -> Translate c m g [LocalPath crumb]
pathsToT q = applySnocPathT pathsToT'
  where
    pathsToT' :: Translate (SnocPath crumb) m g [SnocPath crumb]
    pathsToT' =  collectT (acceptR q >>> contextT)
    {-# INLINE pathsToT' #-}
{-# INLINE pathsToT #-}

-- | Find the 'Path's to every node that satisfies the predicate, ignoring nodes below successes.
prunePathsToT :: forall c crumb g m. (Walker (SnocPath crumb) g, MonadCatch m) => (g -> Bool) -> Translate c m g [LocalPath crumb]
prunePathsToT q = applySnocPathT prunePathsToT'
  where
    prunePathsToT' :: Translate (SnocPath crumb) m g [SnocPath crumb]
    prunePathsToT' =  collectPruneT (acceptR q >>> contextT)
    {-# INLINE prunePathsToT' #-}
{-# INLINE prunePathsToT #-}


onePathToT' :: (Walker (SnocPath crumb) g, MonadCatch m) => (g -> Bool) -> Translate (AbsolutePath crumb) m g (AbsolutePath crumb)
onePathToT' q =  onetdT (acceptR q >>> contextT)

-- | Find the 'Path' to the first node that satisfies the predicate (in a pre-order traversal).
onePathToT :: forall c crumb g m. (Walker (SnocPath crumb) g, MonadCatch m) => (g -> Bool) -> Translate c m g (LocalPath crumb)
onePathToT q = setFailMsg "No matching nodes found." $
               applySnocPathT (onePathToT' q)
{-# INLINE onePathToT #-}

-- | Find the 'Path' to the first descendent node that satisfies the predicate (in a pre-order traversal).
oneNonEmptyPathToT :: (Walker (SnocPath crumb) g, MonadCatch m) => (g -> Bool) -> Translate c m g (LocalPath crumb)
oneNonEmptyPathToT q = setFailMsg "No matching nodes found." $
                       applySnocPathT $ oneT (onePathToT' q)
{-# INLINE oneNonEmptyPathToT #-}


-- local function used by uniquePathToT and uniquePrunePathToT
requireUniquePath :: Monad m => Translate c m [LocalPath crumb] (LocalPath crumb)
requireUniquePath = contextfreeT $ \ ps -> case ps of
                                             []  -> fail "No matching nodes found."
                                             [p] -> return p
                                             _   -> fail $ "Ambiguous: " ++ show (length ps) ++ " matching nodes found."
{-# INLINE requireUniquePath #-}

-- | Find the 'Path' to the node that satisfies the predicate, failing if that does not uniquely identify a node.
uniquePathToT :: (Walker (SnocPath crumb) g, MonadCatch m) => (g -> Bool) -> Translate c m g (LocalPath crumb)
uniquePathToT q = pathsToT q >>> requireUniquePath
{-# INLINE uniquePathToT #-}

-- | Build a 'Path' to the node that satisfies the predicate, failing if that does not uniquely identify a node (ignoring nodes below successes).
uniquePrunePathToT :: (Walker (SnocPath crumb) g, MonadCatch m) => (g -> Bool) -> Translate c m g (LocalPath crumb)
uniquePrunePathToT q = prunePathsToT q >>> requireUniquePath
{-# INLINE uniquePrunePathToT #-}

-------------------------------------------------------------------------------
