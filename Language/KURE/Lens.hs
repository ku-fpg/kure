-- |
-- Module: Language.KURE.Lens
-- Copyright: (c) 2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module defines the KURE 'Lens' type, along with some useful operations.
--
module Language.KURE.Lens
       (  -- * Lenses
          Lens
        , lens
        , lensT
        , focusR
        , focusT
        , pureL
        , failL
        , catchL
        , testLensT
        , bidirectionalL
        , injectL
        , projectL
) where

import Prelude hiding (id, (.))

import Control.Monad
import Control.Category
import Control.Arrow

import Language.KURE.MonadCatch
import Language.KURE.Translate
import Language.KURE.BiTranslate
import Language.KURE.Injection
import Language.KURE.Combinators.Translate

------------------------------------------------------------------------------------------

-- | A 'Lens' is a way to focus on a sub-structure of type @b@ from a structure of type @a@.
newtype Lens c m a b = Lens { -- | Convert a 'Lens' into a 'Translate' that produces a sub-structure (and its context) and an unfocussing function.
                              lensT :: Translate c m a ((c,b), b -> m a)}

-- | The primitive way of building a 'Lens'.
--   If the unfocussing function is applied to the value focussed on then it should succeed,
--   and produce the same value as the original argument (of type @a@).
lens :: Translate c m a ((c,b), b -> m a) -> Lens c m a b
lens = Lens
{-# INLINE lens #-}

-- | Apply a 'Rewrite' at a point specified by a 'Lens'.
focusR :: Monad m => Lens c m a b -> Rewrite c m b -> Rewrite c m a
focusR l r = do ((c,b),k) <- lensT l
                constT (apply r c b >>= k)
{-# INLINE focusR #-}

-- | Apply a 'Translate' at a point specified by a 'Lens'.
focusT :: Monad m => Lens c m a b -> Translate c m b d -> Translate c m a d
focusT l t = do ((c,b),_) <- lensT l
                constT (apply t c b)
{-# INLINE focusT #-}

-- | Check if the focusing succeeds, and additionally whether unfocussing from an unchanged value would succeed.
testLensT :: MonadCatch m => Lens c m a b -> Translate c m a Bool
testLensT l = testM (focusR l id)
{-# INLINE testLensT #-}

instance Monad m => Category (Lens c m) where

-- id :: Lens c m a a
   id = lens $ translate $ \ c a -> return ((c,a), return)
   {-# INLINE id #-}

-- (.) :: Lens c m b d -> Lens c m a b -> Lens c m a d
   l2 . l1 = lens $ translate $ \ ca a -> do ((cb,b),kb) <- apply (lensT l1) ca a
                                             ((cd,d),kd) <- apply (lensT l2) cb b
                                             return ((cd,d),kd >=> kb)
   {-# INLINE (.) #-}

-- | The failing 'Lens'.
failL :: Monad m => String -> Lens c m a b
failL = lens . fail
{-# INLINE failL #-}

-- | A 'Lens' is deemed to have failed (and thus can be caught) if either it fails on the way down, or,
--   crucially, if it would fail on the way up for an unmodified value.  However, actual failure on the way up is not caught
--   (as by then it is too late to use an alternative 'Lens').  This means that, in theory, a use of 'catch' could cause a succeeding 'Lens' application to fail.
--   But provided 'lens' is used correctly, this should never happen.
catchL :: MonadCatch m => Lens c m a b -> (String -> Lens c m a b) -> Lens c m a b
l1 `catchL` l2 = lens (attemptM (focusR l1 idR) >>= either (lensT . l2) (const (lensT l1)))
{-# INLINE catchL #-}

-- | Construct a 'Lens' from a 'BiTranslate'.
bidirectionalL :: Monad m => BiTranslate c m a b -> Lens c m a b
bidirectionalL bt = lens $ do c <- contextT
                              b <- forewardT bt
                              return ((c,b), apply (backwardT bt) c)
{-# INLINE bidirectionalL #-}

-- | Construct a 'Lens' from two pure functions.
pureL :: Monad m => (a -> b) -> (b -> a) -> Lens c m a b
pureL f g = bidirectionalL $ bidirectional (arr f) (arr g)
{-# INLINE pureL #-}

------------------------------------------------------------------------------------------

-- | A 'Lens' to the injection of a value.
injectL  :: (Monad m, Injection a g) => Lens c m a g
injectL = lens $ translate $ \ c a -> return ((c, inject a), projectM)
{-# INLINE injectL #-}

-- | A 'Lens' to the projection of a value.
projectL :: (Monad m, Injection a g) => Lens c m g a
projectL = lens $ translate $ \ c -> projectM >=> (\ a -> return ((c,a), injectM))
{-# INLINE projectL #-}

-------------------------------------------------------------------------------
