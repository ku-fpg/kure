{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
-- |
-- Module: Language.KURE.MonadCatch
-- Copyright: (c) 2012--2014 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides classes for catch-like operations on 'Monad's.

module Language.KURE.MonadCatch
           ( -- * Monads with a Catch
             MonadCatch(..)
             -- ** The KURE Monad
           , KureM
           , runKureM
           , fromKureM
           , liftKureM
             -- ** The IO Monad
           , liftAndCatchIO
             -- ** Combinators
           , (<+)
           , catchesM
           , tryM
           , mtryM
           , attemptM
           , testM
           , notM
           , modFailMsg
           , setFailMsg
           , prefixFailMsg
           , withPatFailMsg
) where

import Prelude hiding (foldr)

import Control.Exception (catch, SomeException)
import Control.Monad
import Control.Monad.IO.Class

import Data.Foldable
import Data.List (isPrefixOf)

import Language.KURE.Combinators.Monad

infixl 3 <+

------------------------------------------------------------------------------------------

-- | 'Monad's with a catch for 'fail'.
--   The following laws are expected to hold:
--
-- > fail msg `catchM` f == f msg
-- > return a `catchM` f == return a

class Monad m => MonadCatch m where
  -- | Catch a failing monadic computation.
  catchM :: m a -> (String -> m a) -> m a

------------------------------------------------------------------------------------------

-- | 'KureM' is the minimal structure that can be an instance of 'MonadCatch'.
--   The KURE user is free to either use 'KureM' or provide their own monad.
--   'KureM' is essentially the same as 'Either' 'String', except that it supports a 'MonadCatch' instance which 'Either' 'String' does not (because its 'fail' method calls 'error')
--   A major advantage of this is that monadic pattern match failures are caught safely.
data KureM a = Failure String | Success a deriving (Eq, Show)

-- | Eliminator for 'KureM'.
runKureM :: (a -> b) -> (String -> b) -> KureM a -> b
runKureM _ f (Failure msg) = f msg
runKureM s _ (Success a)   = s a
{-# INLINE runKureM #-}

-- | Get the value from a 'KureM', providing a function to handle the error case.
fromKureM :: (String -> a) -> KureM a -> a
fromKureM = runKureM id
{-# INLINE fromKureM #-}

-- | Lift a 'KureM' computation to any other monad.
liftKureM :: Monad m => KureM a -> m a
liftKureM = runKureM return fail
{-# INLINE liftKureM #-}

instance Monad KureM where
   return :: a -> KureM a
   return = Success
   {-# INLINE return #-}

   (>>=) :: KureM a -> (a -> KureM b) -> KureM b
   (Success a)   >>= f = f a
   (Failure msg) >>= _ = Failure msg
   {-# INLINE (>>=) #-}

   fail :: String -> KureM a
   fail = Failure
   {-# INLINE fail #-}

instance MonadCatch KureM where
   catchM :: KureM a -> (String -> KureM a) -> KureM a
   (Success a)   `catchM` _ = Success a
   (Failure msg) `catchM` f = f msg
   {-# INLINE catchM #-}

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
ma <+ mb = ma `catchM` (\_ -> mb)
{-# INLINE (<+) #-}

-- | Select the first monadic computation that succeeds, discarding any thereafter.
catchesM :: (Foldable f, MonadCatch m) => f (m a) -> m a
catchesM = foldr (<+) (fail "catchesM failed")
{-# INLINE catchesM #-}

-- | Catch a failing monadic computation, making it succeed with a constant value.
tryM :: MonadCatch m => a -> m a -> m a
tryM a ma = ma <+ return a
{-# INLINE tryM #-}

-- | Catch a failing monadic computation, making it succeed with 'mempty'.
mtryM :: (MonadCatch m, Monoid a) => m a -> m a
mtryM = tryM mempty
{-# INLINE mtryM #-}

-- | Catch a failing monadic computation, making it succeed with an error message.
attemptM :: MonadCatch m => m a -> m (Either String a)
attemptM ma = liftM Right ma `catchM` (return . Left)
{-# INLINE attemptM #-}

-- | Determine if a monadic computation succeeds.
testM :: MonadCatch m => m a -> m Bool
testM ma = liftM (const True) ma <+ return False
{-# INLINE testM #-}

-- | Fail if the monadic computation succeeds; succeed with @()@ if it fails.
notM :: MonadCatch m => m a -> m ()
notM ma = ifM (testM ma) (fail "notM of success") (return ())
{-# INLINE notM #-}

-- | Modify the error message of a failing monadic computation.
--   Successful computations are unaffected.
modFailMsg :: MonadCatch m => (String -> String) -> m a -> m a
modFailMsg f ma = ma `catchM` (fail . f)
{-# INLINE modFailMsg #-}

-- | Set the error message of a failing monadic computation.
--   Successful computations are unaffected.
setFailMsg :: MonadCatch m => String -> m a -> m a
setFailMsg msg = modFailMsg (const msg)
{-# INLINE setFailMsg #-}

-- | Add a prefix to the error message of a failing monadic computation.
--   Successful computations are unaffected.
prefixFailMsg :: MonadCatch m => String -> m a -> m a
prefixFailMsg msg = modFailMsg (msg ++)
{-# INLINE prefixFailMsg #-}

-- | Use the given error message whenever a monadic pattern match failure occurs.
withPatFailMsg :: MonadCatch m => String -> m a -> m a
withPatFailMsg msg = modFailMsg (\ e -> if "Pattern match failure" `isPrefixOf` e then msg else e)
{-# INLINE withPatFailMsg #-}

------------------------------------------------------------------------------------------

-- | The String is generated by 'show'ing the exception.
instance MonadCatch IO where
  catchM :: IO a -> (String -> IO a) -> IO a
  catchM io f = io `catch` (\ e -> f $ show (e :: SomeException))
  {-# INLINE catchM #-}

-- | Lift a computation from the 'IO' monad, catching failures in the target monad.
liftAndCatchIO :: (MonadCatch m, MonadIO m) => IO a -> m a
liftAndCatchIO io = join $ liftIO (liftM return io `catchM` (return . fail))
{-# INLINE liftAndCatchIO #-}

------------------------------------------------------------------------------------------
