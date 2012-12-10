-- |
-- Module: Language.KURE.Catch
-- Copyright: (c) 2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides classes for catch-like operations on 'Monad's and two-paramater classes such as 'Category' and 'Arrow'.

module Language.KURE.Catch
           ( -- * Monads with a Catch
             MonadCatch(..)
             -- ** The KURE Monad
           , KureM
           , runKureM
           , fromKureM
             -- ** Combinators
           , (<<+)
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
             -- * Two-paramater types with Catch and Failure
           , BiCatch(..)
           , (<+)
           , acceptR
           , accepterR
           , tryR
           , changedR
           , repeatR
           , catchesT
) where

import Prelude hiding (id , (.), foldr)

import Control.Applicative
import Control.Monad
import Control.Category
import Control.Arrow

import Data.Foldable
import Data.List (isPrefixOf)
import Data.Monoid

import Language.KURE.Combinators.Arrow
import Language.KURE.Combinators.Monad

infixl 3 <+, <<+

------------------------------------------------------------------------------------------

-- | 'Monad's with a catch for 'fail'.
--   The following law is expected to hold:
--
-- > fail msg `catchM` f == f msg

class Monad m => MonadCatch m where
  -- | Catch a failing monadic computation.
  catchM :: m a -> (String -> m a) -> m a

------------------------------------------------------------------------------------------

-- | 'KureM' is the minimal structure that can be an instance of 'MonadCatch'.
--   The KURE user is free to either use 'KureM' or provide their own monad.
--   'KureM' is essentially the same as ('Either' 'String' @a@), except that the 'fail' method produces an error in the monad,
--   rather than invoking 'error'.
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

instance Monad KureM where
-- return :: a -> KureM a
   return = Success
   {-# INLINE return #-}

-- (>>=) :: KureM a -> (a -> KureM b) -> KureM b
   (Success a)   >>= f = f a
   (Failure msg) >>= _ = Failure msg
   {-# INLINE (>>=) #-}

-- fail :: String -> KureM a
   fail = Failure
   {-# INLINE fail #-}

instance MonadCatch KureM where
-- catchM :: KureM a -> (String -> KureM a) -> KureM a
   (Success a)   `catchM` _ = Success a
   (Failure msg) `catchM` f = f msg
   {-# INLINE catchM #-}

instance Functor KureM where
-- fmap :: (a -> b) -> KureM a -> KureM b
   fmap = liftM
   {-# INLINE fmap #-}

instance Applicative KureM where
-- pure :: a -> KureM a
   pure = return
   {-# INLINE pure #-}

-- (<*>) :: KureM (a -> b) -> KureM a -> KureM b
   (<*>) = ap
   {-# INLINE (<*>) #-}

-------------------------------------------------------------------------------

-- | A monadic catch that ignores the error message.
(<<+) :: MonadCatch m => m a -> m a -> m a
ma <<+ mb = ma `catchM` const mb
{-# INLINE (<<+) #-}

-- | Select the first monadic computation that succeeds, discarding any thereafter.
catchesM :: (Foldable f, MonadCatch m) => f (m a) -> m a
catchesM = foldr (<<+) (fail "catchesM failed")
{-# INLINE catchesM #-}

-- | Catch a failing monadic computation, making it succeed with a constant value.
tryM :: MonadCatch m => a -> m a -> m a
tryM a ma = ma <<+ return a
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
testM ma = liftM (const True) ma <<+ return False
{-# INLINE testM #-}

-- | Fail if the 'Monad' succeeds; succeed with @()@ if it fails.
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

-- | A class of two-paramater type constructors with failure and catching.
--
-- The following law is expected to hold:
--
-- > failT msg `catchT` f == f msg
--
-- Additionally, KURE requires that if @bi a@ is an instance of 'Monad',
-- and bi is an instance of 'BiCatch', then
--
-- > fail == failT

class BiCatch bi where
  -- | Fail with the given error message.
  failT :: String -> bi a b

  -- | Catch an exception with a handler that can depend on the error message.
  catchT :: bi a b -> (String -> bi a b) -> bi a b


-- | Left-biased choice.
(<+) :: BiCatch bi => bi a b -> bi a b -> bi a b
f <+ g = f `catchT` \ _ -> g
{-# INLINE (<+) #-}

-- | Look at the argument to the 'Arrow' before choosing which 'Arrow' to use.
reader :: ArrowApply bi => (a -> bi a b) -> bi a b
reader f = (f &&& id) ^>> app
{-# INLINE reader #-}

-- | Look at the argument to an 'Arrow', and choose to be either the identity arrow or a failure.
acceptWithFailMsgR :: (BiCatch bi, ArrowApply bi) => (a -> Bool) -> String -> bi a a
acceptWithFailMsgR p msg = reader $ \ a -> if p a then id else failT msg
{-# INLINE acceptWithFailMsgR #-}

-- | Look at the argument to an 'Arrow', and choose to be either the identity arrow or a failure.
acceptR :: (BiCatch bi, ArrowApply bi) => (a -> Bool) -> bi a a
acceptR p = acceptWithFailMsgR p "acceptR: predicate failed"
{-# INLINE acceptR #-}

-- | Look at the argument to an 'Arrow', and choose to be either the identity arrow or a failure.
--   This is a generalisation of 'acceptR' to any 'Arrow'.
accepterR :: (BiCatch bi, ArrowApply bi) => bi a Bool -> bi a a
accepterR t = forkFirst t >>> reader (\ (b,a) -> if b then constant a else failT "accepterR: predicate failed")
{-# INLINE accepterR #-}

-- | Catch a failing 'Category', making it into an identity.
tryR :: (BiCatch bi, Category bi) => bi a a -> bi a a
tryR r = r <+ id
{-# INLINE tryR #-}

-- | Makes an 'Arrow' fail if the result value equals the argument value.
changedR :: (BiCatch bi, ArrowApply bi, Eq a) => bi a a -> bi a a
changedR r = reader (\ a -> r >>> acceptWithFailMsgR (/= a) "changedR: value is unchanged")
{-# INLINE changedR #-}

-- | Repeat a 'Category' until it fails, then return the result before the failure.
--   Requires at least the first attempt to succeed.
repeatR :: (BiCatch bi, Category bi) => bi a a -> bi a a
repeatR r = let go = r >>> tryR go
             in go
{-# INLINE repeatR #-}

-- | Select the first 'BiCatch' that succeeds, discarding any thereafter.
catchesT :: (Foldable f, BiCatch bi) => f (bi a b) -> bi a b
catchesT = foldr (<+) (failT "catchesT failed")
{-# INLINE catchesT #-}

------------------------------------------------------------------------------------------
