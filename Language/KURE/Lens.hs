{-# LANGUAGE CPP #-}
{-# LANGUAGE InstanceSigs #-}
-- |
-- Module: Language.KURE.Lens
-- Copyright: (c) 2012--2021 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil.sculthorpe@ntu.ac.uk>
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

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif

import Control.Category
import Control.Arrow

import Language.KURE.MonadCatch
import Language.KURE.Transform
import Language.KURE.BiTransform
import Language.KURE.Injection
import Language.KURE.Combinators.Transform

------------------------------------------------------------------------------------------

-- | A 'Lens' is a way to focus on a sub-structure of type @b@ from a structure of type @a@.
newtype Lens c m a b = Lens { -- | Convert a 'Lens' into a 'Transform' that produces a sub-structure (and its context) and an unfocussing function.
                              lensT :: Transform c m a ((c,b), b -> m a)}

-- | The primitive way of building a 'Lens'.
--   If the unfocussing function is applied to the value focussed on then it should succeed,
--   and produce the same value as the original argument (of type @a@).
lens :: Transform c m a ((c,b), b -> m a) -> Lens c m a b
lens = Lens
{-# INLINE lens #-}

-- | Apply a rewrite at a point specified by a 'Lens'.
focusR :: Monad m => Lens c m a b -> Rewrite c m b -> Rewrite c m a
focusR l r = do ((c,b),k) <- lensT l
                constT (applyR r c b >>= k)
{-# INLINE focusR #-}

-- | Apply a transformation at a point specified by a 'Lens'.
focusT :: Monad m => Lens c m a b -> Transform c m b d -> Transform c m a d
focusT l t = do ((c,b),_) <- lensT l
                constT (applyT t c b)
{-# INLINE focusT #-}

-- | Check if the focusing succeeds, and additionally whether unfocussing from an unchanged value would succeed.
testLensT :: MonadCatch m => Lens c m a b -> Transform c m a Bool
testLensT l = testM (focusR l id)
{-# INLINE testLensT #-}

instance Monad m => Category (Lens c m) where

   id :: Lens c m a a
   id = lens $ transform $ \ c a -> return ((c,a), return)
   {-# INLINE id #-}

   (.) :: Lens c m b d -> Lens c m a b -> Lens c m a d
   l2 . l1 = lens $ transform $ \ ca a -> do ((cb,b),kb) <- applyT (lensT l1) ca a
                                             ((cd,d),kd) <- applyT (lensT l2) cb b
                                             return ((cd,d),kd >=> kb)
   {-# INLINE (.) #-}

-- | The failing 'Lens'.
failL :: MonadFail m => String -> Lens c m a b
failL = lens . fail
{-# INLINE failL #-}

-- | A 'Lens' is deemed to have failed (and thus can be caught) if either it fails on the way down, or,
--   crucially, if it would fail on the way up for an unmodified value.  However, actual failure on the way up is not caught
--   (as by then it is too late to use an alternative 'Lens').  This means that, in theory, a use of 'catchL' could cause a succeeding 'Lens' application to fail.
--   But provided 'lens' is used correctly, this should never happen.
catchL :: MonadCatch m => Lens c m a b -> (String -> Lens c m a b) -> Lens c m a b
l1 `catchL` l2 = lens (attemptM (focusR l1 idR) >>= either (lensT . l2) (const (lensT l1)))
{-# INLINE catchL #-}

-- | Construct a 'Lens' from a 'BiTransform'.
bidirectionalL :: Monad m => BiTransform c m a b -> Lens c m a b
bidirectionalL bt = lens $ do c <- contextT
                              b <- forwardT bt
                              return ((c,b), applyT (backwardT bt) c)
{-# INLINE bidirectionalL #-}

-- | Construct a 'Lens' from two pure functions.
pureL :: Monad m => (a -> b) -> (b -> a) -> Lens c m a b
pureL f g = bidirectionalL $ bidirectional (arr f) (arr g)
{-# INLINE pureL #-}

------------------------------------------------------------------------------------------

-- | A 'Lens' to the injection of a value.
injectL  :: (MonadFail m, Injection a g) => Lens c m a g
injectL = lens $ transform $ \ c a -> return ((c, inject a), projectM)
{-# INLINE injectL #-}

-- | A 'Lens' to the projection of a value.
projectL :: (MonadFail m, Injection a g) => Lens c m g a
projectL = lens $ transform $ \ c -> projectM >=> (\ a -> return ((c,a), injectM))
{-# INLINE projectL #-}

-------------------------------------------------------------------------------
