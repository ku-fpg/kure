{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Language.KURE.Pathfinder
-- Copyright: (c) 2012--2014 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides combinators to find 'LocalPath's sub-nodes specified by a predicate.

module Language.KURE.Pathfinder
        (
        -- * Finding Local Paths
        -- ** Context Transformers
        -- | To find a 'LocalPath' to a node that satisfies a predicate, use @'withLocalPathT' (tt ('acceptLocalPathT' q))@,
        --   where @q@ is a transformation returning 'Bool', and @tt@ is a traversal strategy, such as 'collectT' or 'onetdT'.
        --   This will handle the tracking of the local path.
        --   See the example pathfinders below.
          WithLocalPath
        , withLocalPathT
        , exposeLocalPathT
        , acceptLocalPathT
        -- ** Example Pathfinders
        , pathsToT
        , onePathToT
        , oneNonEmptyPathToT
        , prunePathsToT
        , uniquePathToT
        , uniquePrunePathToT
) where

import Prelude

import Control.Category hiding ((.))
import Control.Arrow
import Control.Monad.Fail (MonadFail)

import Language.KURE.MonadCatch
import Language.KURE.Transform
import Language.KURE.Combinators.Transform
import Language.KURE.Path
import Language.KURE.Walker
import Language.KURE.ExtendableContext

-------------------------------------------------------------------------------

-- | A context transformer that adds a 'LocalPath' (from the current node) to the context.
type WithLocalPath c crumb = ExtendContext c (LocalPath crumb)

-- | Apply a transformation that stores a 'LocalPath' in the context (starting at the current node).
withLocalPathT :: Transform (WithLocalPath c crumb) m a b -> Transform c m a b
withLocalPathT = liftContext (extendContext mempty)
{-# INLINE withLocalPathT #-}

-- | Extract the current 'LocalPath' from the context.
exposeLocalPathT :: Monad m => Transform (WithLocalPath c crumb) m a (LocalPath crumb)
exposeLocalPathT = contextT >>^ extraContext
{-# INLINE exposeLocalPathT #-}

-- | Return the current 'LocalPath' if the predicate transformation succeeds.
acceptLocalPathT :: MonadFail m => Transform c m u Bool -> Transform (WithLocalPath c crumb) m u (LocalPath crumb)
acceptLocalPathT q = accepterR (liftContext baseContext q) >>> exposeLocalPathT
{-# INLINE acceptLocalPathT #-}

-------------------------------------------------------------------------------

-- | Find the 'LocalPath's to every node that satisfies the predicate.
pathsToT :: (Walker (WithLocalPath c crumb) u, MonadCatch m) => Transform c m u Bool -> Transform c m u [LocalPath crumb]
pathsToT q = withLocalPathT (collectT $ acceptLocalPathT q)
{-# INLINE pathsToT #-}

-- | Find the 'LocalPath's to every node that satisfies the predicate, ignoring nodes below successes.
prunePathsToT :: (Walker (WithLocalPath c crumb) u, MonadCatch m) => Transform c m u Bool -> Transform c m u [LocalPath crumb]
prunePathsToT q = withLocalPathT (collectPruneT $ acceptLocalPathT q)
{-# INLINE prunePathsToT #-}

-- | Find the 'LocalPath' to the first node that satisfies the predicate (in a pre-order traversal).
onePathToT :: forall c crumb u m. (Walker (WithLocalPath c crumb) u, MonadCatch m) => Transform c m u Bool -> Transform c m u (LocalPath crumb)
onePathToT q = setFailMsg "No matching nodes found." $
               withLocalPathT (onetdT $ acceptLocalPathT q)
{-# INLINE onePathToT #-}

-- | Find the 'LocalPath' to the first descendent node that satisfies the predicate (in a pre-order traversal).
oneNonEmptyPathToT :: (Walker (WithLocalPath c crumb) u, MonadCatch m) => Transform c m u Bool -> Transform c m u (LocalPath crumb)
oneNonEmptyPathToT q = setFailMsg "No matching nodes found." $
                       withLocalPathT (oneT $ onetdT $ acceptLocalPathT q)
{-# INLINE oneNonEmptyPathToT #-}


-- local function used by uniquePathToT and uniquePrunePathToT
requireUniquePath :: MonadFail m => Transform c m [LocalPath crumb] (LocalPath crumb)
requireUniquePath = contextfreeT $ \ ps -> case ps of
                                             []  -> fail "No matching nodes found."
                                             [p] -> return p
                                             _   -> fail $ "Ambiguous: " ++ show (length ps) ++ " matching nodes found."
{-# INLINE requireUniquePath #-}

-- | Find the 'LocalPath' to the node that satisfies the predicate, failing if that does not uniquely identify a node.
uniquePathToT :: (Walker (WithLocalPath c crumb) u, MonadCatch m) => Transform c m u Bool -> Transform c m u (LocalPath crumb)
uniquePathToT q = pathsToT q >>> requireUniquePath
{-# INLINE uniquePathToT #-}

-- | Build a 'LocalPath' to the node that satisfies the predicate, failing if that does not uniquely identify a node (ignoring nodes below successes).
uniquePrunePathToT :: (Walker (WithLocalPath c crumb) u, MonadCatch m) => Transform c m u Bool -> Transform c m u (LocalPath crumb)
uniquePrunePathToT q = prunePathsToT q >>> requireUniquePath
{-# INLINE uniquePrunePathToT #-}

-------------------------------------------------------------------------------
