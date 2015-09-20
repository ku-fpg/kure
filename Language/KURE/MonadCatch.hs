{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module: Language.KURE.MonadCatch
-- Copyright: (c) 2012--2015 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides classes for catch-like operations on 'Monad's.

module Language.KURE.MonadCatch
           ( -- * Monads with a Catch
             MonadThrow(..)
           , MonadCatch(..)
           , MonadMask(..)
             -- ** The KURE Monad
           , KureM
           , runKureM
           , runAndShowKureM
           , fromKureM
           , liftKureM
           , throwKureM
             -- ** The IO Monad
           , liftAndCatchIO
             -- ** Combinators
           , (<+)
           , (<+>)
           , catchesM
           , tryM
           , mtryM
           , attemptM
           , testM
           , notM
           , modExc
           , setExc
           , withPatFailExc
) where

import Prelude hiding (foldr)

import Control.Exception (PatternMatchFail(..))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Foldable
import Data.Typeable

import Language.KURE.Combinators.Monad
import Language.KURE.Exceptions

infixl 3 <+
infixl 3 <+>

------------------------------------------------------------------------------------------

-- | 'KureM' is the minimal structure that can be an instance of 'MonadCatch'.
--   The KURE user is free to either use 'KureM' or provide their own monad.
--
--   'KureM' is essentially the same as @'Either' 'SomeException'@.
data KureM a = Failure SomeException | Success a deriving (Show, Typeable)

-- | Eliminator for 'KureM'.
runKureM :: (a -> b) -> (SomeException -> b) -> KureM a -> b
runKureM _ f (Failure e) = f e
runKureM s _ (Success a) = s a
{-# INLINE runKureM #-}

-- | Eliminator for 'KureM' which 'show's the result if successful, and uses
--   'showKureExc' if unsuccessful.
runAndShowKureM :: Show a => KureM a -> String
runAndShowKureM = runKureM (\a -> "KURE Success: " ++ show a)
                           (\e -> "KURE Failure: " ++ showKureExc e)
{-# INLINE runAndShowKureM #-}

-- | Lift a 'KureM' computation to any other monad.
liftKureM :: Monad m => KureM a -> m a
liftKureM = runKureM return (fail . showKureExc)
{-# INLINE liftKureM #-}

-- | Lift a 'KureM' computation to any monad which supports throwing exceptions.
throwKureM :: MonadThrow m => KureM a -> m a
throwKureM = runKureM return throwM
{-# INLINE throwKureM #-}

-- | Get the value from a 'KureM', given a function to handle the exception case.
fromKureM :: (SomeException -> a) -> KureM a -> a
fromKureM = runKureM id
{-# INLINE fromKureM #-}

instance Monad KureM where
   return :: a -> KureM a
   return = Success
   {-# INLINE return #-}

   (>>=) :: KureM a -> (a -> KureM b) -> KureM b
   Failure e >>= _ = Failure e
   Success a >>= f = f a
   {-# INLINE (>>=) #-}

   -- | Produces a 'Failure' containing a 'PatternMatchFail'.
   fail :: String -> KureM a
   fail = Failure . SomeException . PatternMatchFail
   {-# INLINE fail #-}

-- | Produces a 'Failure' containing the exception.
instance MonadThrow KureM where
   throwM :: Exception e => e -> KureM a
   throwM = Failure . toException
   {-# INLINE throwM #-}

instance MonadCatch KureM where
   catch :: Exception e => KureM a -> (e -> KureM a) -> KureM a
   m@(Failure e) `catch` f =
       case fromException e of
            Just ex -> f ex
            Nothing -> m
   Success a `catch` _ = Success a
   {-# INLINE catch #-}

instance MonadMask KureM where
   mask :: ((forall a. KureM a -> KureM a) -> KureM b) -> KureM b
   mask f = f id
   {-# INLINE mask #-}

   uninterruptibleMask :: ((forall a. KureM a -> KureM a) -> KureM b) -> KureM b
   uninterruptibleMask f = f id
   {-# INLINE uninterruptibleMask #-}

instance Functor KureM where
   fmap :: (a -> b) -> KureM a -> KureM b
   fmap = liftM
   {-# INLINE fmap #-}

instance Applicative KureM where
   pure :: a -> KureM a
   pure = return
   {-# INLINE pure #-}

   (<*>) :: KureM (a -> b) -> KureM a -> KureM b
   (<*>) = ap
   {-# INLINE (<*>) #-}

-------------------------------------------------------------------------------

-- | A monadic catch that ignores the error message.
(<+) :: MonadCatch m => m a -> m a -> m a
ma <+ mb = ma `catch` \SomeException{} -> mb
{-# INLINE (<+) #-}

-- | Catches a monadic action only if it throws a 'NodeMismatch'.
--   Intended for combining mutually exclusive congruence combinators.
(<+>) :: MonadCatch m => m a -> m a -> m a
ma <+> mb = ma `catch` \NodeMismatch{} -> mb
{-# INLINE (<+>) #-}

-- | Select the first monadic computation that succeeds, discarding any thereafter.
catchesM :: (Foldable f, MonadCatch m) => f (m a) -> m a
catchesM = foldr (<+) (throwM $ strategyFailure "catchesM")
{-# INLINE catchesM #-}

-- | Catch a exception-throwing monadic computation, making it succeed with a constant value.
tryM :: MonadCatch m => a -> m a -> m a
tryM a ma = ma <+ return a
{-# INLINE tryM #-}

-- | Catch an exception-throwing monadic computation, making it succeed with 'mempty'.
mtryM :: (MonadCatch m, Monoid a) => m a -> m a
mtryM = tryM mempty
{-# INLINE mtryM #-}

-- | Catch an exception-throwing monadic computation, making it succeed with an error message.
attemptM :: (Exception e, MonadCatch m) => m a -> m (Either e a)
attemptM ma = liftM Right ma `catch` (return . Left)
{-# INLINE attemptM #-}

-- | Determine if a monadic computation succeeds.
testM :: MonadCatch m => m a -> m Bool
testM ma = liftM (const True) ma <+ return False
{-# INLINE testM #-}

-- | Fail if the monadic computation succeeds; succeed with @()@ if it fails.
notM :: MonadCatch m => m a -> m ()
notM ma = ifM (testM ma)
              (throwM $ strategyFailure "notM")
              (return ())
{-# INLINE notM #-}

-- | Modify the exception of an exception-throwing monadic computation.
--   Successful computations are unaffected.
modExc :: (Exception e1, Exception e2, MonadCatch m) => (e1 -> e2) -> m a -> m a
modExc f ma = ma `catch` (throwM . f)
{-# INLINE modExc #-}

-- | Set the exception of an exception-throwing monadic computation.
--   Successful computations are unaffected.
setExc :: (Exception e, MonadCatch m) => e -> m a -> m a
setExc e = modExc $ \SomeException{} -> e
{-# INLINE setExc #-}

-- | Use the given exception whenever a monadic pattern-match failure occurs.
withPatFailExc :: (Exception e, MonadCatch m) => e -> m a -> m a
withPatFailExc e = modExc $ \PatternMatchFail{} -> e

------------------------------------------------------------------------------------------

-- | Lift a computation from the 'IO' monad, catching exceptions in the target monad.
liftAndCatchIO :: (MonadCatch m, MonadIO m) => IO a -> m a
liftAndCatchIO io = join $ liftIO (liftM return io `catch` \(SomeException e) -> return (throwM e))
{-# INLINE liftAndCatchIO #-}
