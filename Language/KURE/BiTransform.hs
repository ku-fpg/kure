{-# Language InstanceSigs #-}
-- |
-- Module: Language.KURE.BiTransform
-- Copyright: (c) 2012--2014 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- A bi-directional transformation is a transformation that can be applied in either direction.

module Language.KURE.BiTransform
       (  -- * Bi-directional Transformations
          BiTransform, BiTranslate
        , BiRewrite
        , bidirectional
        , forwardT
        , backwardT
        , whicheverR
        , invertBiT
        , beforeBiR
) where

import Prelude hiding (id, (.))

import Control.Category

import Language.KURE.MonadCatch
import Language.KURE.Transform

------------------------------------------------------------------------------------------

-- | An undirected 'Transform'.
data BiTransform c m a b = BiTransform {forwardT :: Transform c m a b, -- ^ Extract the forward 'Transform' from a 'BiTransform'.
                                        backwardT :: Transform c m b a  -- ^ Extract the backward 'Transform' from a 'BiTransform'.
                                       }

-- | A deprecated synonym for 'BiTranslate'.
type BiTranslate c m a b = BiTransform c m a b

-- | A 'BiTransform' that shares the same source and target type.
type BiRewrite c m a = BiTransform c m a a

-- | Construct a 'BiTransform' from two opposite 'Transform's.
bidirectional :: Transform c m a b -> Transform c m b a -> BiTransform c m a b
bidirectional = BiTransform
{-# INLINE bidirectional #-}

-- | Try the 'BiRewrite' forwards, then backwards if that fails.
--   Useful when you know which rule you want to apply, but not which direction to apply it in.
whicheverR :: MonadCatch m => BiRewrite c m a -> Rewrite c m a
whicheverR r = forwardT r <+ backwardT r
{-# INLINE whicheverR #-}

-- | Invert the forwards and backwards directions of a 'BiTransform'.
invertBiT :: BiTransform c m a b -> BiTransform c m b a
invertBiT (BiTransform t1 t2) = BiTransform t2 t1
{-# INLINE invertBiT #-}

instance Monad m => Category (BiTransform c m) where
   id :: BiTransform c m a a
   id = bidirectional id id
   {-# INLINE id #-}

   (.) :: BiTransform c m b d -> BiTransform c m a b -> BiTransform c m a d
   (BiTransform f1 b1) . (BiTransform f2 b2) = BiTransform (f1 . f2) (b2 . b1)
   {-# INLINE (.) #-}


-- | Perform the argument transformation before /either/ direction of the bidirectional rewrite.
beforeBiR :: Monad m => Transform c m a b -> (b -> BiRewrite c m a) -> BiRewrite c m a
beforeBiR t f = bidirectional (t >>= (forwardT . f)) (t >>= (backwardT . f))
{-# INLINE beforeBiR #-}

------------------------------------------------------------------------------------------
