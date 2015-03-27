{-# LANGUAGE CPP #-}
-- |
-- Module: Language.KURE.Combinators.Arrow
-- Copyright: (c) 2012--2014 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides some utility arrow routing combinators.

module Language.KURE.Combinators.Arrow
           ( -- * Arrow Routing
             -- | The names 'result' and 'argument' are taken from Conal Elliott's semantic editor combinators.
             --   <http://conal.net/blog/posts/semantic-editor-combinators>
             result
           , argument
           , toFst
           , toSnd
           , swap
           , fork
           , forkFirst
           , forkSecond
           , constant
           , serialise
           , parallelise
) where

import Prelude hiding (id, foldr)

import Control.Category hiding ((.))
import Control.Arrow

#if __GLASGOW_HASKELL__ <= 708
import Data.Monoid
#endif
import Data.Foldable

------------------------------------------------------------------------------------------

-- | Apply a pure function to the result of an arrow.
result :: Arrow bi => (b -> c) -> bi a b -> bi a c
result f a = a >>^ f
{-# INLINE result #-}

-- | Apply a pure function to the argument to an arrow.
argument :: Arrow bi => (a -> b) -> bi b c -> bi a c
argument f a = f ^>> a
{-# INLINE argument #-}

-- | Apply an arrow to the first element of a pair, discarding the second element.
toFst :: Arrow bi => bi a b -> bi (a,x) b
toFst f = fst ^>> f
{-# INLINE toFst #-}

-- | Apply an arrow to the second element of a pair, discarding the first element.
toSnd :: Arrow bi => bi a b -> bi (x,a) b
toSnd f = snd ^>> f
{-# INLINE toSnd #-}

-- | A pure arrow that swaps the elements of a pair.
swap :: Arrow bi => bi (a,b) (b,a)
swap = arr (\(a,b) -> (b,a))
{-# INLINE swap #-}

-- | A pure arrow that duplicates its argument.
fork :: Arrow bi => bi a (a,a)
fork = arr (\a -> (a,a))
{-# INLINE fork #-}

-- | Tag the result of an arrow with its argument.
forkFirst :: Arrow bi => bi a b -> bi a (b,a)
forkFirst sf = fork >>> first sf
{-# INLINE forkFirst #-}

-- | Tag the result of an arrow with its argument.
forkSecond :: Arrow bi => bi a b -> bi a (a,b)
forkSecond sf = fork >>> second sf
{-# INLINE forkSecond #-}

-- | An arrow with a constant result.
constant :: Arrow bi => b -> bi a b
constant = arr . const
{-# INLINE constant #-}

-------------------------------------------------------------------------------

-- | Sequence (from left to right) a collection of 'Category's.
serialise :: (Foldable f, Category bi) => f (bi a a) -> bi a a
serialise = foldr (>>>) id
{-# INLINE serialise #-}

-- | Apply a collection of arrows to the same input, combining their results in a monoid.
parallelise :: (Foldable f, Arrow bi, Monoid b) => f (bi a b) -> bi a b
parallelise = foldr (\ f g -> (f &&& g) >>^ uncurry mappend) (constant mempty)
{-# INLINE parallelise #-}

-------------------------------------------------------------------------------
