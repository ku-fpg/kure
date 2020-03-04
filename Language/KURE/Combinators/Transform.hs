{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module: Language.KURE.Combinators.Transform
-- Copyright: (c) 2012--2015 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides a variety of combinators over 'Transform' and 'Rewrite'.

module Language.KURE.Combinators.Transform
        ( -- * Transformation Combinators
          idR
        , successT
        , failT
        , contextT
        , exposeT
        , liftContext
        , readerT
        , resultT
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
        , acceptWithFailExcR
        , accepterR
        , changedR
        , changedByR
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
import Control.Monad(ap,liftM)
import Control.Monad.Catch
import qualified Control.Monad.Fail as Fail

import Language.KURE.Combinators.Arrow
import Language.KURE.Combinators.Monad
import Language.KURE.Exceptions
import Language.KURE.MonadCatch
import Language.KURE.Transform

------------------------------------------------------------------------------------------

-- | The identity rewrite.
idR :: Monad m => Rewrite c m a
idR = id
{-# INLINE idR #-}

-- | An always successful transformation.
successT :: Applicative m => Transform c m a ()
successT = pure ()
{-# INLINE successT #-}

-- | An always failing tranformation.
failT :: MonadThrow m => Transform c m a b
failT = constT (throwM $ strategyFailure "failT")
{-# INLINE failT #-}

-- | Extract the current context.
contextT :: Applicative m => Transform c m a c
contextT = contextonlyT pure
{-# INLINE contextT #-}

-- | Expose the current context and value.
exposeT :: Applicative m => Transform c m a (c,a)
exposeT = transform (curry pure)
{-# INLINE exposeT #-}

-- | Lift a transformation to operate on a derived context.
liftContext :: (c -> c') -> Transform c' m a b -> Transform c m a b
liftContext f t = transform (applyT t . f)
{-# INLINE liftContext #-}

-- | Map a transformation over a list.
mapT :: (Traversable t, Applicative m) => Transform c m a b -> Transform c m (t a) (t b)
mapT t = transform (traverse . applyT t)
{-# INLINE mapT #-}

-- | An identity rewrite with side-effects.
sideEffectR :: Monad m => (c -> a -> m ()) -> Rewrite c m a
sideEffectR f = transform f >> id
{-# INLINE sideEffectR #-}

-- | Look at the argument to the transformation before choosing which 'Transform' to use.
readerT :: (a -> Transform c m a b) -> Transform c m a b
readerT f = transform (\ c a -> applyT (f a) c a)
{-# INLINE readerT #-}

-- | Convert the monadic result of a transformation into a result in another monad.
resultT :: (m b -> n d) -> Transform c m a b -> Transform c n a d
resultT f t = transform (\ c -> f . applyT t c)
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

-- | As 'acceptR', but takes a custom exception.
acceptWithFailExcR :: (Exception e, MonadThrow m) => (a -> Bool) -> e -> Rewrite c m a
acceptWithFailExcR p e = readerT $ \ a -> if p a then id else throwM e
{-# INLINE acceptWithFailExcR #-}

-- | Look at the argument to a rewrite, and choose to be either 'idR' or a failure.
acceptR :: MonadThrow m => (a -> Bool) -> Rewrite c m a
acceptR p = acceptWithFailExcR p (strategyFailure "acceptR")
{-# INLINE acceptR #-}

-- | A generalisation of 'acceptR' where the predicate is a 'Transform'.
accepterR :: MonadThrow m => Transform c m a Bool -> Rewrite c m a
accepterR t = ifM t idR (throwM $ strategyFailure "accepterR")
{-# INLINE accepterR #-}

-- | Catch a failing rewrite, making it into an identity.
tryR :: MonadCatch m => Rewrite c m a -> Rewrite c m a
tryR r = r <+ id
{-# INLINE tryR #-}

-- | Makes a rewrite fail if the result value and the argument value
--   satisfy the equality predicate. This is a generalisation of 'changedR'.
--   @changedR = changedByR ('==')@
changedByR :: MonadCatch m => (a -> a -> Bool) -> Rewrite c m a -> Rewrite c m a
changedByR p r = readerT (\ a -> r >>> acceptWithFailExcR (not . p a) (strategyFailure "changedByR"))
{-# INLINE changedByR #-}

-- | Makes an rewrite throw an exception if the result value equals the argument value.
changedR :: (MonadCatch m, Eq a) => Rewrite c m a -> Rewrite c m a
changedR = changedByR (==)
{-# INLINE changedR #-}

-- | Repeat a rewrite until it fails, then return the result before
--   the failure. Requires at least the first attempt to succeed.
repeatR :: MonadCatch m => Rewrite c m a -> Rewrite c m a
repeatR r = let go = r >>> tryR go
             in go
{-# INLINE repeatR #-}

-- | An identity transformation that resembles a monadic 'Control.Monad.join'.
joinT :: Transform c m (m a) a
joinT = contextfreeT id
{-# INLINE joinT #-}

-- | Fail if the Boolean is False, succeed if the Boolean is True.
guardT :: MonadThrow m => Transform c m Bool ()
guardT = contextfreeT guardM
{-# INLINE guardT #-}

-------------------------------------------------------------------------------

data PBool a = PBool !Bool a

instance Functor PBool where
  fmap :: (a -> b) -> PBool a -> PBool b
  fmap f (PBool b a) = PBool b (f a)

checkSuccessPBool :: (Exception e, MonadThrow m) => e -> m (PBool a) -> m a
checkSuccessPBool e m = do PBool b a <- m
                           if b
                             then pure a
                             else throwM e
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

instance Monad m => Functor (AnyR m) where
   fmap :: (a -> b) -> AnyR m a -> AnyR m b
   fmap = liftM
   {-# INLINE fmap #-}

instance Monad m => Applicative (AnyR m) where
   pure :: a -> AnyR m a
   pure = AnyR . pure . PBool False
   {-# INLINE pure #-}

   (<*>) :: AnyR m (a -> b) -> AnyR m a -> AnyR m b
   (<*>) = ap
   {-# INLINE (<*>) #-}

instance Monad m => Monad (AnyR m) where
   (>>=) :: AnyR m a -> (a -> AnyR m d) -> AnyR m d
   ma >>= f = AnyR $ do PBool b1 a <- unAnyR ma
                        PBool b2 d <- unAnyR (f a)
                        return (PBool (b1 || b2) d)
   {-# INLINE (>>=) #-}

instance Fail.MonadFail m => Fail.MonadFail (AnyR m) where
   fail :: String -> AnyR m a
   fail = AnyR . Fail.fail
   {-# INLINE fail #-}

instance MonadThrow m => MonadThrow (AnyR m) where
   throwM :: Exception e => e -> AnyR m a
   throwM = AnyR . throwM
   {-# INLINE throwM #-}

instance MonadCatch m => MonadCatch (AnyR m) where
   catch :: Exception e => AnyR m a -> (e -> AnyR m a) -> AnyR m a
   catch ma f = AnyR (unAnyR ma `catch` (unAnyR . f))
   {-# INLINE catch #-}

instance MonadMask m => MonadMask (AnyR m) where
   mask :: ((forall a. AnyR m a -> AnyR m a) -> AnyR m b) -> AnyR m b
   mask f = AnyR $ mask $ \u -> unAnyR (f $ q u)
     where q :: (m (PBool a) -> m (PBool a)) -> AnyR m a -> AnyR m a
           q u = AnyR . u . unAnyR
   {-# INLINE mask #-}

   uninterruptibleMask :: ((forall a. AnyR m a -> AnyR m a) -> AnyR m b) -> AnyR m b
   uninterruptibleMask f = AnyR $ uninterruptibleMask $ \u -> unAnyR (f $ q u)
     where q :: (m (PBool a) -> m (PBool a)) -> AnyR m a -> AnyR m a
           q u = AnyR . u . unAnyR
   {-# INLINE uninterruptibleMask #-}

-- | Wrap a 'Rewrite' using the 'AnyR' monad transformer.
wrapAnyR :: MonadCatch m => Rewrite c m a -> Rewrite c (AnyR m) a
wrapAnyR r = rewrite $ \ c a -> AnyR $ (PBool True <$> applyR r c a) <+ return (PBool False a)
{-# INLINE wrapAnyR #-}

-- | Unwrap a 'Rewrite' from the 'AnyR' monad transformer.
unwrapAnyR :: MonadThrow m => Rewrite c (AnyR m) a -> Rewrite c m a
unwrapAnyR = resultT (checkSuccessPBool (strategyFailure "anyR") . unAnyR)
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

instance Monad m => Functor (OneR m) where
   fmap :: (a -> b) -> OneR m a -> OneR m b
   fmap = liftM
   {-# INLINE fmap #-}

instance Monad m => Applicative (OneR m) where
   pure :: a -> OneR m a
   pure a = OneR (\ b -> pure (PBool b a))
   {-# INLINE pure #-}

   (<*>) :: OneR m (a -> b) -> OneR m a -> OneR m b
   (<*>) = ap
   {-# INLINE (<*>) #-}

instance Monad m => Monad (OneR m) where
   (>>=) :: OneR m a -> (a -> OneR m d) -> OneR m d
   ma >>= f = OneR $ \ b1 -> do PBool b2 a <- unOneR ma b1
                                unOneR (f a) b2
   {-# INLINE (>>=) #-}

instance Fail.MonadFail m => Fail.MonadFail (OneR m) where
   fail :: String -> OneR m a
   fail msg = OneR (\ _ -> Fail.fail msg)
   {-# INLINE fail #-}

instance MonadThrow m => MonadThrow (OneR m) where
   throwM :: Exception e => e -> OneR m a
   throwM e = OneR $ \_ -> throwM e
   {-# INLINE throwM #-}

instance MonadCatch m => MonadCatch (OneR m) where
   catch :: Exception e => OneR m a -> (e -> OneR m a) -> OneR m a
   catch (OneR g) f = OneR (\ b -> g b `catch` (($ b) . unOneR . f))
   {-# INLINE catch #-}

instance MonadMask m => MonadMask (OneR m) where
   mask :: ((forall a. OneR m a -> OneR m a) -> OneR m b) -> OneR m b
   mask f = OneR $ \b -> mask $ \u -> unOneR (f $ q u) b
     where q :: (m (PBool a) -> m (PBool a)) -> OneR m a -> OneR m a
           q u pb = OneR $ u . unOneR pb
   {-# INLINE mask #-}

   uninterruptibleMask :: ((forall a. OneR m a -> OneR m a) -> OneR m b) -> OneR m b
   uninterruptibleMask f = OneR $ \b -> uninterruptibleMask $ \u -> unOneR (f $ q u) b
     where q :: (m (PBool a) -> m (PBool a)) -> OneR m a -> OneR m a
           q u pb = OneR $ u . unOneR pb
   {-# INLINE uninterruptibleMask #-}

-- | Wrap a 'Rewrite' using the 'OneR' monad transformer.
wrapOneR :: MonadCatch m => Rewrite c m g -> Rewrite c (OneR m) g
wrapOneR r = rewrite $ \ c a -> OneR $ \ b -> if b
                                                then return (PBool True a)
                                                else (PBool True <$> applyR r c a) <+ return (PBool False a)
{-# INLINE wrapOneR #-}

-- | Unwrap a 'Rewrite' from the 'OneR' monad transformer.
unwrapOneR :: MonadThrow m => Rewrite c (OneR m) a -> Rewrite c m a
unwrapOneR = resultT (checkSuccessPBool (strategyFailure "oneR") . ($ False) . unOneR)
{-# INLINE unwrapOneR #-}

-------------------------------------------------------------------------------
