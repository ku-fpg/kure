-- |
-- Module: Language.KURE.BiTranslate
-- Copyright: (c) 2012--2013 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- A bi-directional translation is a tranlsation that can be applied in either direction.

module Language.KURE.BiTranslate
       (  -- * Bi-directional Translations
          BiTranslate
        , BiRewrite
        , bidirectional
        , forewardT
        , backwardT
        , whicheverR
        , invert
) where

import Prelude hiding (id, (.))

import Control.Category

import Language.KURE.MonadCatch
import Language.KURE.Translate

------------------------------------------------------------------------------------------

-- | An undirected 'Translate'.
data BiTranslate c m a b = BiTranslate {forewardT :: Translate c m a b, -- ^ Extract the foreward 'Translate' from a 'BiTranslate'.
                                        backwardT :: Translate c m b a  -- ^ Extract the backward 'Translate' from a 'BiTranslate'.
                                       }

-- | A 'BiTranslate' that shares the same source and target type.
type BiRewrite c m a = BiTranslate c m a a

-- | Construct a 'BiTranslate' from two opposite 'Translate's.
bidirectional :: Translate c m a b -> Translate c m b a -> BiTranslate c m a b
bidirectional = BiTranslate
{-# INLINE bidirectional #-}

-- | Try the 'BiRewrite' forewards, then backwards if that fails.
--   Useful when you know which rule you want to apply, but not which direction to apply it in.
whicheverR :: MonadCatch m => BiRewrite c m a -> Rewrite c m a
whicheverR r = forewardT r <+ backwardT r
{-# INLINE whicheverR #-}

-- | Invert the forewards and backwards directions of a 'BiTranslate'.
invert :: BiTranslate c m a b -> BiTranslate c m b a
invert (BiTranslate t1 t2) = BiTranslate t2 t1
{-# INLINE invert #-}

instance Monad m => Category (BiTranslate c m) where
-- id :: BiTranslate c m a a
   id = bidirectional id id
   {-# INLINE id #-}

-- (.) :: BiTranslate c m b d -> BiTranslate c m a b -> BiTranslate c m a d
   (BiTranslate f1 b1) . (BiTranslate f2 b2) = BiTranslate (f1 . f2) (b2 . b1)
   {-# INLINE (.) #-}

------------------------------------------------------------------------------------------
