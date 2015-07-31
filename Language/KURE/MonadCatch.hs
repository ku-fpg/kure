{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
             MonadThrow(..)
           , MonadCatch(..)
           , MonadMask(..)
             -- ** The KURE Monad
           , KureM
           , caseKureM
           , runKureM
           , runExceptionKureM
           , fromKureM
           , liftKureM
           , throwKureM
             -- ** Exceptions
           , MsgException
           , modMsgException
           , toMsgException
           , fromMsgException
           , mkMsgException
           , runMsgException
           , KureException(..)
             -- ** The IO Monad
           , liftAndCatchIO
             -- ** Combinators
           , (<+)
           , (<+>)
           , catchM
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

import Control.Exception (PatternMatchFail(..))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Foldable
import Data.Typeable

import Language.KURE.Combinators.Monad

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
import Data.Monoid
#endif

infixl 3 <+

------------------------------------------------------------------------------------------

-- | 'KureM' is a minimal structure that can be an instance of 'MonadCatch'
--   and provide aggregating error messages. The KURE user is free to either
--   use 'KureM' or provide their own monad.
--
--   'KureM' is essentially the same as @'Either' 'MsgException'@.
data KureM a = Failure MsgException | Success a deriving (Show, Typeable)

-- | Eliminator for 'KureM'.
caseKureM :: (a -> b) -> (MsgException -> b) -> KureM a -> b
caseKureM _ f (Failure e) = f e
caseKureM s _ (Success a) = s a
{-# INLINE caseKureM #-}

-- | Eliminator for 'KureM' that only uses the error message of the 'MsgException'.
runKureM :: (a -> b) -> (String -> b) -> KureM a -> b
runKureM s f = caseKureM s (f . fst . runMsgException)
{-# INLINE runKureM #-}

-- | Eliminator for 'KureM' that ignores the error message of the 'MsgException'.
runExceptionKureM :: (a -> b) -> (SomeException -> b) -> KureM a -> b
runExceptionKureM s f = caseKureM s (f . snd . runMsgException)

-- | Lift a 'KureM' computation to any other monad.
liftKureM :: Monad m => KureM a -> m a
liftKureM = runKureM return fail
{-# INLINE liftKureM #-}

-- | Lift a 'KureM' computation to any monad which supports throwing exceptions.
throwKureM :: MonadThrow m => KureM a -> m a
throwKureM = runExceptionKureM return throwM
{-# INLINE throwKureM #-}

-- | Get the value from a 'KureM', providing a function to handle the error case.
fromKureM :: (String -> a) -> KureM a -> a
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

   -- | Yields a 'MsgException' with a 'PatternMatchFail'.
   fail :: String -> KureM a
   fail str = Failure $ mkMsgException str (PatternMatchFail str)
   {-# INLINE fail #-}

-- | Throws a 'MsgException' with a blank error message.
instance MonadThrow KureM where
   throwM :: Exception e => e -> KureM a
   throwM = Failure . toMsgException
   {-# INLINE throwM #-}

instance MonadCatch KureM where
   catch :: Exception e => KureM a -> (e -> KureM a) -> KureM a
   m@(Failure me) `catch` f =
       case fromMsgException me of
            Just ex -> f ex
            Nothing -> m
   Success a `catch` _ = Success a
   {-# INLINE catch #-}

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

-- | Another name for 'catch'.
catchM :: (Exception e, MonadCatch m) => m a -> (e -> m a) -> m a
catchM = catch
{-# INLINE catchM #-}

-- | A monadic catch that ignores the error message.
(<+) :: MonadCatch m => m a -> m a -> m a
ma <+ mb = ma `catch` \(_ :: SomeException) -> mb
{-# INLINE (<+) #-}

-- | Catches the a monadic action only if it throws a 'KureException'.
(<+>) :: MonadCatch m => m a -> m a -> m a
ma <+> mb = ma `catch` \KureException -> mb
{-# INLINE (<+>) #-}

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
attemptM :: (Exception e, MonadCatch m) => m a -> m (Either e a)
attemptM ma = liftM Right ma `catch` (return . Left)
{-# INLINE attemptM #-}

-- | Determine if a monadic computation succeeds.
testM :: MonadCatch m => m a -> m Bool
testM ma = liftM (const True) ma <+ return False
{-# INLINE testM #-}

-- | Fail if the monadic computation succeeds; succeed with @()@ if it fails.
notM :: MonadCatch m => m a -> m ()
notM ma = ifM (testM ma) (fail "notM of success") (return ())
{-# INLINE notM #-}

-- | Modify the error message of a failing monadic computation using 'MsgException',
--   which has the ability to catch any exception. Successful computations are unaffected.
modFailMsg :: MonadCatch m => (String -> String) -> m a -> m a
modFailMsg f ma = ma `catch` \me@MsgException{} -> throwM (modMsgException f me)
{-# INLINE modFailMsg #-}

-- | Set the error message of a failing monadic computation using 'MsgException'.
--   Successful computations are unaffected.
setFailMsg :: MonadCatch m => String -> m a -> m a
setFailMsg ex = modFailMsg (const ex)
{-# INLINE setFailMsg #-}

-- | Add a prefix to the error message of a failing monadic computation using
--   'MsgException'. Successful computations are unaffected.
prefixFailMsg :: MonadCatch m => String -> m a -> m a
prefixFailMsg msg = modFailMsg (msg ++)
{-# INLINE prefixFailMsg #-}

-- | Use the given error message whenever a monadic pattern match failure occurs.
-- If @'withPatFailMsg' msg ma@ catches an exception, a 'MsgException' is thrown
-- with @msg@ as the error message and 'KureException' as the underlying exception.
withPatFailMsg :: MonadCatch m => String -> m a -> m a
withPatFailMsg msg ma = ma `catch` \PatternMatchFail{} -> throwM (mkMsgException msg KureException)
{-# INLINE withPatFailMsg #-}

------------------------------------------------------------------------------------------

-- | Lift a computation from the 'IO' monad, catching exceptions in the target monad.
liftAndCatchIO :: (MonadCatch m, MonadIO m) => IO a -> m a
liftAndCatchIO io = join $ liftIO (liftM return io `catch` \(se :: SomeException) -> return (throwM se))
{-# INLINE liftAndCatchIO #-}

------------------------------------------------------------------------------------------

-- | A 'MsgException' is an error message plus 'SomeException'. Like 'SomeException',
-- 'MsgException' can be used to catch any 'Exception':
--
-- @
-- ma `catch` \\'MsgException'{} -> mb = mb
-- @
--
-- In addition, the underlying 'Exception' of a 'MsgException' may be caught:
--
-- @
-- throwM ('toMsgException' ('PatternMatchFail' "")) `catch` \\'PatternMatchFail'{} -> mb = mb
-- @
--
-- 'MsgException' is useful because not all 'Exception's have error messages. Using
-- a 'MsgException' permits access to an error message that can be accessed upon
-- successively thrown exceptions.
data MsgException = MsgException String SomeException deriving (Show, Typeable)

instance Exception MsgException where
    toException = snd . runMsgException
    fromException (SomeException se) = Just $ toMsgException se

modMsgException :: (String -> String) -> MsgException -> MsgException
modMsgException f (MsgException msg se) = MsgException (f msg) se
{-# INLINE modMsgException #-}

-- | Make a 'MsgException' from an error message and an arbitrary 'Exception'.
-- If the 'Exception' argument is a 'MsgException', the underlying 'Exception'
-- will be preserved, but the underlying error message will be replaced with
-- the 'String' argument.
mkMsgException :: Exception e => String -> e -> MsgException
mkMsgException msg e = case cast e of
    Just (MsgException _ se) -> MsgException msg se
    Nothing                  -> MsgException msg $ toException e
{-# INLINE mkMsgException #-}

-- | Make a 'MsgException' from an arbitrary 'Exception'. If the 'Exception'
--   argument is a 'MsgException', the underlying error message and 'Exception'
--   will be preserved. Otherwise, a 'MsgException' will be created with a
--   blank error message.
toMsgException :: Exception e => e -> MsgException
toMsgException e = case cast e of
    Just me -> me
    Nothing -> MsgException "" $ toException e
{-# INLINe toMsgException #-}

-- | Retrieve the error message and underlying exception of a 'MsgException'.
runMsgException :: MsgException -> (String, SomeException)
runMsgException (MsgException msg se) = (msg, se)
{-# INLINE runMsgException #-}

-- | Casts the underlying exception of a 'MsgException'. If being casted to a
--   'MsgException', the error message will be preserved.
fromMsgException :: Exception e => MsgException -> Maybe e
fromMsgException me@(MsgException _ se) = case cast me of
    Just e  -> Just e
    Nothing -> fromException se
{-# INLINE fromMsgException #-}

------------------------------------------------------------------------------------------

-- | A congurence combinator failure.
data KureException = KureException deriving (Eq, Show, Typeable)
instance Exception KureException
