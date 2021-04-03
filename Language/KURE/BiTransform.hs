{-# LANGUAGE InstanceSigs #-}
-- |
-- Module: Language.KURE.BiTransform
-- Copyright: (c) 2012--2021 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil.sculthorpe@ntu.ac.uk>
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
        , extractWithFailMsgBiT
        , promoteWithFailMsgBiT
        , extractWithFailMsgBiR
        , promoteWithFailMsgBiR
) where

import Prelude hiding (id, (.))

import Control.Category
import Control.Monad.Fail (MonadFail)

import Language.KURE.MonadCatch
import Language.KURE.Transform
import Language.KURE.Injection

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

-- | As 'extractBiT', but takes a custom error message to use if extraction fails.
extractWithFailMsgBiT :: (MonadFail m, Injection a u, Injection b u) => String -> BiTransform c m u u -> BiTransform c m a b
extractWithFailMsgBiT msg (BiTransform t1 t2) = BiTransform (extractT t1 >>> projectWithFailMsgT msg)
                                                            (extractT t2 >>> projectWithFailMsgT msg)
{-# INLINE extractWithFailMsgBiT #-}

-- | Convert a bidirectional transformation over an injected value into a bidirectional transformation over non-injected values,
--   (failing if an injected value cannot be projected).
extractBiT :: (MonadFail m, Injection a u, Injection b u) => BiTransform c m u u -> BiTransform c m a b
extractBiT = extractWithFailMsgBiT "extractBiT failed"
{-# INLINE extractBiT #-}

-- | As 'promoteBiT', but takes a custom error message to use if promotion fails.
promoteWithFailMsgBiT  :: (MonadFail m, Injection a u, Injection b u) => String -> BiTransform c m a b -> BiTransform c m u u
promoteWithFailMsgBiT msg (BiTransform t1 t2) = BiTransform (projectWithFailMsgT msg >>> t1 >>> injectT)
                                                            (projectWithFailMsgT msg >>> t2 >>> injectT)
{-# INLINE promoteWithFailMsgBiT #-}

-- | Promote a bidirectional transformation from value to value into a transformation over an injection of those values,
--   (failing if an injected value cannot be projected).
promoteBiT  :: (MonadFail m, Injection a u, Injection b u) => BiTransform c m a b -> BiTransform c m u u
promoteBiT = promoteWithFailMsgBiT "promoteBiT failed"
{-# INLINE promoteBiT #-}

-- | As 'extractBiR', but takes a custom error message to use if extraction fails.
extractWithFailMsgBiR :: (MonadFail m, Injection a u) => String -> BiRewrite c m u -> BiRewrite c m a
extractWithFailMsgBiR msg (BiTransform r1 r2) = BiTransform (extractWithFailMsgR msg r1)
                                                            (extractWithFailMsgR msg r2)
{-# INLINE extractWithFailMsgBiR #-}

-- | Convert a bidirectional rewrite over an injected value into a bidirectional rewrite over a projection of that value,
--   (failing if an injected value cannot be projected).
extractBiR :: (MonadFail m, Injection a u) => BiRewrite c m u -> BiRewrite c m a
extractBiR = extractWithFailMsgBiR "extractBiR failed"
{-# INLINE extractBiR #-}

-- | As 'promoteBiR', but takes a custom error message to use if promotion fails.
promoteWithFailMsgBiR :: (MonadFail m, Injection a u) => String -> BiRewrite c m a -> BiRewrite c m u
promoteWithFailMsgBiR msg (BiTransform r1 r2) = BiTransform (promoteWithFailMsgR msg r1)
                                                            (promoteWithFailMsgR msg r2)
{-# INLINE promoteWithFailMsgBiR #-}

-- | Promote a bidirectional rewrite over a value into a bidirectional rewrite over an injection of that value,
--   (failing if an injected value cannot be projected).
promoteBiR :: (MonadFail m, Injection a u) => BiRewrite c m a -> BiRewrite c m u
promoteBiR = promoteWithFailMsgBiR "promoteBiR failed"
{-# INLINE promoteBiR #-}

------------------------------------------------------------------------------------------
