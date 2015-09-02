{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs #-}
-- |
-- Module: Language.KURE.BiTransform
-- Copyright: (c) 2012--2015 The University of Kansas
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
        , afterBiR
          -- * Bi-directional Injections
        , extractBiT
        , promoteBiT
        , extractBiR
        , promoteBiR
        , extractWithFailExcBiT
        , promoteWithFailExcBiT
        , extractWithFailExcBiR
        , promoteWithFailExcBiR
) where

import Prelude hiding (id, (.))

import Control.Category
import Control.Monad.Catch

#if __GLASGOW_HASKELL__ >= 708
import Data.Typeable
#endif

import Language.KURE.Exceptions
import Language.KURE.Injection
import Language.KURE.MonadCatch
import Language.KURE.Transform

------------------------------------------------------------------------------------------

-- | An undirected 'Transform'.
data BiTransform c m a b = BiTransform {forwardT :: Transform c m a b, -- ^ Extract the forward 'Transform' from a 'BiTransform'.
                                        backwardT :: Transform c m b a  -- ^ Extract the backward 'Transform' from a 'BiTransform'.
                                       }
#if __GLASGOW_HASKELL__ >= 708
  deriving Typeable
#endif

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

------------------------------------------------------------------------------------------

-- | Perform the argument transformation before /either/ direction of the bidirectional rewrite.
beforeBiR :: Monad m => Transform c m a b -> (b -> BiRewrite c m a) -> BiRewrite c m a
beforeBiR t f = bidirectional (t >>= (forwardT . f)) (t >>= (backwardT . f))
{-# INLINE beforeBiR #-}

-- | Apply the argument rewrite to the result of /either/ direction of the bidirectional rewrite.
afterBiR :: Monad m => BiRewrite c m a -> Rewrite c m a -> BiRewrite c m a
afterBiR b rr = bidirectional (forwardT b >>> rr) (backwardT b >>> rr)
{-# INLINE afterBiR #-}

------------------------------------------------------------------------------------------

-- | As 'extractBiT', but takes a custom exception to use if extraction fails.
extractWithFailExcBiT :: (Exception e, MonadThrow m, Injection a u, Injection b u) => e -> BiTransform c m u u -> BiTransform c m a b
extractWithFailExcBiT e (BiTransform t1 t2) = BiTransform (extractT t1 >>> projectWithFailExcT e)
                                                          (extractT t2 >>> projectWithFailExcT e)
{-# INLINE extractWithFailExcBiT #-}

-- | Convert a bidirectional transformation over an injected value into a bidirectional transformation over non-injected values,
--   (throwing an exception if an injected value cannot be projected).
extractBiT :: (MonadThrow m, Injection a u, Injection b u) => BiTransform c m u u -> BiTransform c m a b
extractBiT = extractWithFailExcBiT $ strategyFailure "extractBiT"
{-# INLINE extractBiT #-}

-- | As 'promoteBiT', but takes a custom exception to use if promotion fails.
promoteWithFailExcBiT  :: (Exception e, MonadThrow m, Injection a u, Injection b u) => e -> BiTransform c m a b -> BiTransform c m u u
promoteWithFailExcBiT e (BiTransform t1 t2) = BiTransform (projectWithFailExcT e >>> t1 >>> injectT)
                                                          (projectWithFailExcT e >>> t2 >>> injectT)
{-# INLINE promoteWithFailExcBiT #-}

-- | Promote a bidirectional transformation from value to value into a transformation over an injection of those values,
--   (throwing an exception if an injected value cannot be projected).
promoteBiT  :: (MonadThrow m, Injection a u, Injection b u) => BiTransform c m a b -> BiTransform c m u u
promoteBiT = promoteWithFailExcBiT $ strategyFailure "promoteBiT"
{-# INLINE promoteBiT #-}

-- | As 'extractBiR', but takes a custom exception to use if extraction fails.
extractWithFailExcBiR :: (Exception e, MonadThrow m, Injection a u) => e -> BiRewrite c m u -> BiRewrite c m a
extractWithFailExcBiR e (BiTransform r1 r2) = BiTransform (extractWithFailExcR e r1)
                                                          (extractWithFailExcR e r2)
{-# INLINE extractWithFailExcBiR #-}

-- | Convert a bidirectional rewrite over an injected value into a bidirectional rewrite over a projection of that value,
--   (throwing an exception if an injected value cannot be projected).
extractBiR :: (MonadThrow m, Injection a u) => BiRewrite c m u -> BiRewrite c m a
extractBiR = extractWithFailExcBiR $ strategyFailure "extractBiR"
{-# INLINE extractBiR #-}

-- | As 'promoteBiR', but takes a custom exception to use if promotion fails.
promoteWithFailExcBiR :: (Exception e, MonadThrow m, Injection a u) => e -> BiRewrite c m a -> BiRewrite c m u
promoteWithFailExcBiR e (BiTransform r1 r2) = BiTransform (promoteWithFailExcR e r1)
                                                          (promoteWithFailExcR e r2)
{-# INLINE promoteWithFailExcBiR #-}

-- | Promote a bidirectional rewrite over a value into a bidirectional rewrite over an injection of that value,
--   (failing if an injected value cannot be projected).
promoteBiR :: (MonadThrow m, Injection a u) => BiRewrite c m a -> BiRewrite c m u
promoteBiR = promoteWithFailExcBiR $ strategyFailure "promoteBiR"
{-# INLINE promoteBiR #-}

------------------------------------------------------------------------------------------
