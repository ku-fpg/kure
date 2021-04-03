{-# LANGUAGE InstanceSigs #-}
-- |
-- Module: Language.KURE.Combinators.Transform
-- Copyright: (c) 2012--2021 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil.sculthorpe@ntu.ac.uk>
-- Stability: beta
-- Portability: ghc
--
-- This module provides a variety of combinators over 'Transform' and 'Rewrite'.

module Language.KURE.Combinators.Transform
        ( -- * Transformation Combinators
          idR
        , successT
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
        , acceptWithFailMsgR
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
import Control.Monad (liftM,ap)
import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail

import Data.Foldable ()
import Data.Traversable

import Language.KURE.Combinators.Arrow
import Language.KURE.Combinators.Monad
import Language.KURE.MonadCatch
import Language.KURE.Transform

------------------------------------------------------------------------------------------

-- | The identity rewrite.
idR :: Monad m => Rewrite c m a
idR = id
{-# INLINE idR #-}

-- | An always successful transformation.
successT :: Monad m => Transform c m a ()
successT = return ()
{-# INLINE successT #-}

-- | Extract the current context.
contextT :: Monad m => Transform c m a c
contextT = transform (\ c _ -> return c)
{-# INLINE contextT #-}

-- | Expose the current context and value.
exposeT :: Monad m => Transform c m a (c,a)
exposeT = transform (curry return)
{-# INLINE exposeT #-}

-- | Lift a transformation to operate on a derived context.
liftContext :: (c -> c') -> Transform c' m a b -> Transform c m a b
liftContext f t = transform (applyT t . f)
{-# INLINE liftContext #-}

-- | Map a transformation over a list.
mapT :: (Traversable t, Monad m) => Transform c m a b -> Transform c m (t a) (t b)
mapT t = transform (mapM . applyT t)
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

-- | As 'acceptR', but takes a custom failure message.
acceptWithFailMsgR :: MonadFail m => (a -> Bool) -> String -> Rewrite c m a
acceptWithFailMsgR p msg = readerT $ \ a -> if p a then id else fail msg
{-# INLINE acceptWithFailMsgR #-}

-- | Look at the argument to a rewrite, and choose to be either 'idR' or a failure.
acceptR :: MonadFail m => (a -> Bool) -> Rewrite c m a
acceptR p = acceptWithFailMsgR p "acceptR: predicate failed"
{-# INLINE acceptR #-}

-- | A generalisation of 'acceptR' where the predicate is a 'Transform'.
accepterR :: MonadFail m => Transform c m a Bool -> Rewrite c m a
accepterR t = ifM t idR (fail "accepterR: predicate failed")
{-# INLINE accepterR #-}

-- | Catch a failing rewrite, making it into an identity.
tryR :: MonadCatch m => Rewrite c m a -> Rewrite c m a
tryR r = r <+ id
{-# INLINE tryR #-}

-- | Makes a rewrite fail if the result value and the argument value satisfy the equality predicate.
--   This is a generalisation of 'changedR'.
--   @changedR = changedByR ('==')@
changedByR :: MonadCatch m => (a -> a -> Bool) -> Rewrite c m a -> Rewrite c m a
changedByR p r = readerT (\ a -> r >>> acceptWithFailMsgR (not . p a) "changedByR: value is unchanged")
{-# INLINE changedByR #-}

-- | Makes an rewrite fail if the result value equals the argument value.
changedR :: (MonadCatch m, Eq a) => Rewrite c m a -> Rewrite c m a
changedR = changedByR (==)
{-# INLINE changedR #-}

-- | Repeat a rewrite until it fails, then return the result before the failure.
--   Requires at least the first attempt to succeed.
repeatR :: MonadCatch m => Rewrite c m a -> Rewrite c m a
repeatR r = let go = r >>> tryR go
             in go
{-# INLINE repeatR #-}

-- | Attempt each transformation until one succeeds, then return that result and discard the rest of the transformations.
catchesT :: MonadCatch m => [Transform c m a b] -> Transform c m a b
catchesT = catchesM
{-# INLINE catchesT #-}
{-# DEPRECATED catchesT "Please use 'catchesM' instead." #-}


-- | An identity transformation that resembles a monadic 'Control.Monad.join'.
joinT :: Transform c m (m a) a
joinT = contextfreeT id
{-# INLINE joinT #-}

-- | Fail if the Boolean is False, succeed if the Boolean is True.
guardT :: MonadFail m => Transform c m Bool ()
guardT = contextfreeT guardM
{-# INLINE guardT #-}

-------------------------------------------------------------------------------

data PBool a = PBool !Bool a

instance Functor PBool where
  fmap :: (a -> b) -> PBool a -> PBool b
  fmap f (PBool b a) = PBool b (f a)

checkSuccessPBool :: MonadFail m => String -> m (PBool a) -> m a
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

instance Monad m => Functor (AnyR m) where
   fmap :: (a -> b) -> AnyR m a -> AnyR m b
   fmap = liftM
   {-# INLINE fmap #-}

instance Monad m => Applicative (AnyR m) where
   pure :: a -> AnyR m a
   pure = return
   {-# INLINE pure #-}

   (<*>) :: AnyR m (a -> b) -> AnyR m a -> AnyR m b
   (<*>) = ap
   {-# INLINE (<*>) #-}

instance Monad m => Monad (AnyR m) where
   return :: a -> AnyR m a
   return = AnyR . return . PBool False
   {-# INLINE return #-}

   (>>=) :: AnyR m a -> (a -> AnyR m d) -> AnyR m d
   ma >>= f = AnyR $ do PBool b1 a <- unAnyR ma
                        PBool b2 d <- unAnyR (f a)
                        return (PBool (b1 || b2) d)
   {-# INLINE (>>=) #-}

   fail :: String -> AnyR m a
   fail = AnyR . fail
   {-# INLINE fail #-}

instance MonadFail m => MonadFail (AnyR m) where
   fail :: String -> AnyR m a
   fail = AnyR . fail
   {-# INLINE fail #-}

instance MonadCatch m => MonadCatch (AnyR m) where
   catchM :: AnyR m a -> (String -> AnyR m a) -> AnyR m a
   catchM ma f = AnyR (unAnyR ma `catchM` (unAnyR . f))
   {-# INLINE catchM #-}

-- | Wrap a 'Rewrite' using the 'AnyR' monad transformer.
wrapAnyR :: MonadCatch m => Rewrite c m a -> Rewrite c (AnyR m) a
wrapAnyR r = rewrite $ \ c a -> AnyR $ (PBool True `liftM` applyR r c a) <+ return (PBool False a)
{-# INLINE wrapAnyR #-}

-- | Unwrap a 'Rewrite' from the 'AnyR' monad transformer.
unwrapAnyR :: MonadFail m => Rewrite c (AnyR m) a -> Rewrite c m a
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

instance Monad m => Functor (OneR m) where
   fmap :: (a -> b) -> OneR m a -> OneR m b
   fmap = liftM
   {-# INLINE fmap #-}

instance Monad m => Applicative (OneR m) where
   pure :: a -> OneR m a
   pure = return
   {-# INLINE pure #-}

   (<*>) :: OneR m (a -> b) -> OneR m a -> OneR m b
   (<*>) = ap
   {-# INLINE (<*>) #-}

instance Monad m => Monad (OneR m) where
   return :: a -> OneR m a
   return a = OneR (\ b -> return (PBool b a))
   {-# INLINE return #-}

   (>>=) :: OneR m a -> (a -> OneR m d) -> OneR m d
   ma >>= f = OneR $ \ b1 -> do PBool b2 a <- unOneR ma b1
                                unOneR (f a) b2
   {-# INLINE (>>=) #-}

   fail :: String -> OneR m a
   fail msg = OneR (\ _ -> fail msg)
   {-# INLINE fail #-}

instance MonadFail m => MonadFail (OneR m) where
   fail :: String -> OneR m a
   fail msg = OneR (\ _ -> fail msg)
   {-# INLINE fail #-}

instance MonadCatch m => MonadCatch (OneR m) where
   catchM :: OneR m a -> (String -> OneR m a) -> OneR m a
   catchM (OneR g) f = OneR (\ b -> g b `catchM` (($ b) . unOneR . f))
   {-# INLINE catchM #-}

-- | Wrap a 'Rewrite' using the 'OneR' monad transformer.
wrapOneR :: MonadCatch m => Rewrite c m g -> Rewrite c (OneR m) g
wrapOneR r = rewrite $ \ c a -> OneR $ \ b -> if b
                                                then return (PBool True a)
                                                else (PBool True `liftM` applyR r c a) <+ return (PBool False a)
{-# INLINE wrapOneR #-}

-- | Unwrap a 'Rewrite' from the 'OneR' monad transformer.
unwrapOneR :: MonadFail m => Rewrite c (OneR m) a -> Rewrite c m a
unwrapOneR = resultT (checkSuccessPBool "oneR failed" . ($ False) . unOneR)
{-# INLINE unwrapOneR #-}

-------------------------------------------------------------------------------
