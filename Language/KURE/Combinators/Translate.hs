-- |
-- Module: Language.KURE.Combinators.Translate
-- Copyright: (c) 2012--2013 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides a variety of combinators over 'Translate' and 'Rewrite'.

module Language.KURE.Combinators.Translate
        ( -- * Translate Combinators
          idR
        , contextT
        , exposeT
        , liftContext
        , readerT
        , resultT
        , catchesT
        , mapT
        , joinT
        , guardT
          -- * Rewrite Combinators
        , tryR
        , andR
        , orR
        , (>+>)
        , repeatR
        , acceptR
        , accepterR
        , changedR
        , sideEffectR
          -- * Monad Transformers
          -- ** anyR Support
          -- $AnyR_doc
        , AnyR
        , wrapAnyR
        , unwrapAnyR
          -- ** oneR Support
          -- $OneR_doc
        , OneR
        , wrapOneR
        , unwrapOneR
) where

import Prelude hiding (id, map, foldr, mapM)

import Control.Category ((>>>),id)
import Control.Monad (liftM)

import Data.Foldable
import Data.Traversable

import Language.KURE.Combinators.Arrow
import Language.KURE.Combinators.Monad
import Language.KURE.MonadCatch
import Language.KURE.Translate

------------------------------------------------------------------------------------------

-- | The identity 'Rewrite'.
idR :: Monad m => Rewrite c m a
idR = id
{-# INLINE idR #-}

-- | Extract the current context.
contextT :: Monad m => Translate c m a c
contextT = translate (\ c _ -> return c)
{-# INLINE contextT #-}

-- | Expose the current context and value.
exposeT :: Monad m => Translate c m a (c,a)
exposeT = translate (curry return)
{-# INLINE exposeT #-}

-- | Apply a 'Translate' in a modified context.
liftContext :: (c -> c') -> Translate c' m a b -> Translate c m a b
liftContext f t = translate (apply t . f)
{-# INLINE liftContext #-}

-- | Map a 'Translate' over a list.
mapT :: (Traversable t, Monad m) => Translate c m a b -> Translate c m (t a) (t b)
mapT t = translate (mapM . apply t)
{-# INLINE mapT #-}

-- | An identity 'Rewrite' with side-effects.
sideEffectR :: Monad m => (c -> a -> m ()) -> Rewrite c m a
sideEffectR f = translate f >> id
{-# INLINE sideEffectR #-}

-- | Look at the argument to the 'Translate' before choosing which 'Translate' to use.
readerT :: (a -> Translate c m a b) -> Translate c m a b
readerT f = translate (\ c a -> apply (f a) c a)
{-# INLINE readerT #-}

-- | Convert the monadic result of a 'Translate' into a result in another monad.
resultT :: (m b -> n d) -> Translate c m a b -> Translate c n a d
resultT f t = translate (\ c -> f . apply t c)
{-# INLINE resultT #-}

-- | Perform a collection of rewrites in sequence, requiring all to succeed.
andR :: (Foldable f, Monad m) => f (Rewrite c m a) -> Rewrite c m a
andR = serialise
{-# INLINE andR #-}

-- | Perform two rewrites in sequence, succeeding if one or both succeed.
(>+>) :: MonadCatch m => Rewrite c m a -> Rewrite c m a -> Rewrite c m a
r1 >+> r2 = unwrapAnyR (wrapAnyR r1 >>> wrapAnyR r2)
{-# INLINE (>+>) #-}

-- | Perform a collection of rewrites in sequence, succeeding if any succeed.
orR :: (Functor f, Foldable f, MonadCatch m) => f (Rewrite c m a) -> Rewrite c m a
orR = unwrapAnyR . andR . fmap wrapAnyR
{-# INLINE orR #-}

-- | As 'acceptR', but takes a custom failure message.
acceptWithFailMsgR :: Monad m => (a -> Bool) -> String -> Rewrite c m a
acceptWithFailMsgR p msg = readerT $ \ a -> if p a then id else fail msg
{-# INLINE acceptWithFailMsgR #-}

-- | Look at the argument to an 'Rewrite', and choose to be either the identity rewrite or a failure.
acceptR :: Monad m => (a -> Bool) -> Rewrite c m a
acceptR p = acceptWithFailMsgR p "acceptR: predicate failed"
{-# INLINE acceptR #-}

-- | A generalisation of 'acceptR' where the predicate is a 'Translate'.
accepterR :: Monad m => Translate c m a Bool -> Rewrite c m a
accepterR t = ifM t idR (fail "accepterR: predicate failed")
{-# INLINE accepterR #-}

-- | Catch a failing 'Category', making it into an identity.
tryR :: MonadCatch m => Rewrite c m a -> Rewrite c m a
tryR r = r <+ id
{-# INLINE tryR #-}

-- | Makes an 'Rewrite' fail if the result value equals the argument value.
changedR :: (MonadCatch m, Eq a) => Rewrite c m a -> Rewrite c m a
changedR r = readerT (\ a -> r >>> acceptWithFailMsgR (/= a) "changedR: value is unchanged")
{-# INLINE changedR #-}

-- | Repeat a 'Rewrite' until it fails, then return the result before the failure.
--   Requires at least the first attempt to succeed.
repeatR :: MonadCatch m => Rewrite c m a -> Rewrite c m a
repeatR r = let go = r >>> tryR go
             in go
{-# INLINE repeatR #-}

-- | Attempt each 'Translate' until one succeeds, then return that result and discard the rest of the 'Translate's.
catchesT :: MonadCatch m => [Translate c m a b] -> Translate c m a b
catchesT = foldr (<+) (fail "catchesT failed")
{-# INLINE catchesT #-}

-- | An identity translation that resembles a monadic 'join'.
joinT :: Translate c m (m a) a
joinT = contextfreeT id
{-# INLINE joinT #-}

-- | Fail if the Boolean is False, succeed if the Boolean is True.
guardT :: Monad m => Translate c m Bool ()
guardT = contextfreeT guardM
{-# INLINE guardT #-}

-------------------------------------------------------------------------------

data PBool a = PBool !Bool a

checkSuccessPBool :: Monad m => String -> m (PBool a) -> m a
checkSuccessPBool msg m = do PBool b a <- m
                             if b
                               then return a
                               else fail msg
{-# INLINE checkSuccessPBool #-}

-------------------------------------------------------------------------------

-- $AnyR_doc
-- These are useful when defining congruence combinators that succeed if /any/ child rewrite succeeds.
-- See the \"Expr\" example, or the HERMIT package.

-- | The 'AnyR' transformer, in combination with 'wrapAnyR' and 'unwrapAnyR',
--   causes a sequence of rewrites to succeed if at least one succeeds, converting failures to
--   identity rewrites.
newtype AnyR m a = AnyR (m (PBool a))

unAnyR :: AnyR m a -> m (PBool a)
unAnyR (AnyR mba) = mba
{-# INLINE unAnyR #-}

instance Monad m => Monad (AnyR m) where
-- return :: a -> AnyR m a
   return = AnyR . return . PBool False
   {-# INLINE return #-}

-- fail :: String -> AnyR m a
   fail = AnyR . fail
   {-# INLINE fail #-}

-- (>>=) :: AnyR m a -> (a -> AnyR m d) -> AnyR m d
   ma >>= f = AnyR $ do PBool b1 a <- unAnyR ma
                        PBool b2 d <- unAnyR (f a)
                        return (PBool (b1 || b2) d)
   {-# INLINE (>>=) #-}

instance MonadCatch m => MonadCatch (AnyR m) where
-- catchM :: AnyR m a -> (String -> AnyR m a) -> AnyR m a
   catchM ma f = AnyR (unAnyR ma `catchM` (unAnyR . f))
   {-# INLINE catchM #-}

-- | Wrap a 'Rewrite' using the 'AnyR' monad transformer.
wrapAnyR :: MonadCatch m => Rewrite c m a -> Rewrite c (AnyR m) a
wrapAnyR r = rewrite $ \ c a -> AnyR $ (PBool True `liftM` apply r c a) <+ return (PBool False a)
{-# INLINE wrapAnyR #-}

-- | Unwrap a 'Rewrite' from the 'AnyR' monad transformer.
unwrapAnyR :: Monad m => Rewrite c (AnyR m) a -> Rewrite c m a
unwrapAnyR = resultT (checkSuccessPBool "anyR failed" . unAnyR)
{-# INLINE unwrapAnyR #-}

-------------------------------------------------------------------------------

-- $OneR_doc
-- These are useful when defining congruence combinators that succeed if one child rewrite succeeds
-- (and the remainder are then discarded).
-- See the \"Expr\" example, or the HERMIT package.

-- | The 'OneR' transformer, in combination with 'wrapOneR' and 'unwrapOneR',
--   causes a sequence of rewrites to only apply the first success, converting the remainder (and failures) to identity rewrites.
newtype OneR m a = OneR (Bool -> m (PBool a))

unOneR :: OneR m a -> Bool -> m (PBool a)
unOneR (OneR mba) = mba
{-# INLINE unOneR #-}

instance Monad m => Monad (OneR m) where
-- return :: a -> OneR m a
   return a = OneR (\ b -> return (PBool b a))
   {-# INLINE return #-}

-- fail :: String -> OneR m a
   fail msg = OneR (\ _ -> fail msg)
   {-# INLINE fail #-}

-- (>>=) :: OneR m a -> (a -> OneR m d) -> OneR m d
   ma >>= f = OneR $ \ b1 -> do PBool b2 a <- unOneR ma b1
                                unOneR (f a) b2
   {-# INLINE (>>=) #-}

instance MonadCatch m => MonadCatch (OneR m) where
-- catchM :: OneR m a -> (String -> OneR m a) -> OneR m a
   catchM (OneR g) f = OneR (\ b -> g b `catchM` (($ b) . unOneR . f))
   {-# INLINE catchM #-}

-- | Wrap a 'Rewrite' using the 'OneR' monad transformer.
wrapOneR :: MonadCatch m => Rewrite c m g -> Rewrite c (OneR m) g
wrapOneR r = rewrite $ \ c a -> OneR $ \ b -> if b
                                                then return (PBool True a)
                                                else (PBool True `liftM` apply r c a) <+ return (PBool False a)
{-# INLINE wrapOneR #-}

-- | Unwrap a 'Rewrite' from the 'OneR' monad transformer.
unwrapOneR :: Monad m => Rewrite c (OneR m) a -> Rewrite c m a
unwrapOneR = resultT (checkSuccessPBool "oneR failed" . ($ False) . unOneR)
{-# INLINE unwrapOneR #-}

-------------------------------------------------------------------------------
