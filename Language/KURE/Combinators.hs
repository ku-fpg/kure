{-# LANGUAGE TypeOperators, TupleSections #-}

-- |
-- Module: Language.KURE.Combinators
-- Copyright: (c) 2006-2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: alpha
-- Portability: ghc
--
-- This module provides various monadic and arrow combinators that are particularly useful for
-- the 'Translate' 'Monad'/'Arrow'.
-- Note that these combinators assume that 'mplus' behaves as a catch, for both 'fail' and 'mzero'.

module Language.KURE.Combinators
           (
             -- | 'Monad' combinators
             guardFail
           , condM
           , whenM
           , tryM
           , mtryM
           , attemptM
           , testM
           , notM
           , mconcatM
           , memptyM
             -- | 'Arrow' combinators
           , result
           , argument
           , idR
           , (<+)
           , readerR
           , acceptR
           , tryR
           , attemptR
           , changedR
           , repeatR
           , (>+>)
           , orR
           , andR
) where

import Prelude hiding (id , (.))
import Control.Monad
import Control.Category
import Control.Arrow
import Data.Monoid
import Data.Maybe (isJust)

infixl 3 <+, >+>

------------------------------------------------------------------------------------------

-- | similar to 'guard', but using 'fail' rather than 'mzero'.
guardFail ::  Monad m => Bool -> String -> m ()
guardFail b msg = if b then return () else fail msg

-- | if-then-else lifted over a 'Monad'.
condM ::  Monad m => m Bool -> m a -> m a -> m a
condM mb m1 m2 = do b <- mb
                    if b then m1 else m2

-- | if-then lifted over a 'Monad'.
whenM ::  Monad m => m Bool -> m a -> m a
whenM mb ma = condM mb ma (fail "condition False")

-- | catch a failing monadic computation, making it succeed with a constant value.
tryM :: MonadPlus m => a -> m a -> m a
tryM a ma = ma `mplus` return a

-- | catch a failing monadic computation, making it succeed with 'mempty'.
mtryM :: (MonadPlus m, Monoid a) => m a -> m a
mtryM = tryM mempty

-- | catch a failing monadic computation, making it succeed with 'Nothing'.
attemptM :: MonadPlus m => m a -> m (Maybe a)
attemptM = tryM Nothing . liftM Just

-- | determine if a monadic computation succeeds.
testM :: MonadPlus m => m a -> m Bool
testM = liftM isJust . attemptM

-- | 'notT' fails if the 'Monad' succeeds, and succeeds with @()@ if it fails.
notM :: MonadPlus m => m a -> m ()
notM ma = attemptM ma >>= maybe (return ()) (const mzero)

------------------------------------------------------------------------------------------

-- | performs a list of 'Applicative's in order, then combines their result in a 'Monoid'.
mconcatM :: (Monad m , Monoid a) => [m a] -> m a
mconcatM = liftM mconcat . sequence

-- | 'memptyA' always succeeds with 'mempty'
memptyM :: (Monad m, Monoid a) => m a
memptyM = return mempty

------------------------------------------------------------------------------------------

-- | Names taken from Conal Elliott's semantic editor combinators.

-- | apply a pure function to the result of an 'Arrow'.
result :: Arrow (~>) => (b -> c) -> (a ~> b) -> (a ~> c)
result f a = a >>^ f

-- | apply a pure function to the argument to an 'Arrow'.
argument :: Arrow (~>) => (a -> b) -> (b ~> c) -> (a ~> c)
argument f a = f ^>> a

-------------------------------------------------------------------------------

-- | synonym for 'id'.
idR :: Category (~>) => (a ~> a)
idR = id

-- | synonym for '<+>'.
(<+) :: ArrowPlus (~>) => (a ~> b) -> (a ~> b) -> (a ~> b)
(<+) = (<+>)

-- | look at the argument to the 'Arrow' before choosing which 'Translate' to perform.
readerR :: ArrowApply (~>) => (a -> (a ~> b)) -> (a ~> b)
readerR f = (f &&& id) ^>> app

-- | look at the argument to an 'Arrow', and choose to be either a failure or trivial success.
acceptR :: (ArrowZero (~>), ArrowApply (~>)) => (a -> Bool) -> (a ~> a)
acceptR p = readerR $ \ a -> if p a then id else zeroArrow

-- | catch a failing 'ArrowPlus', making it into an identity.
tryR :: ArrowPlus (~>) => (a ~> a) -> (a ~> a)
tryR r = r <+> id

-- | catch a failing 'ArrowPlus', making it succeed with a Boolean flag.
--   Useful when defining 'anyR' instances.
attemptR :: ArrowPlus (~>) => (a ~> a) -> (a ~> (Bool,a))
attemptR r = (r >>^ (True,)) <+> arr (False,)

-- | makes an 'Arrow' fail if the result value equals the argument value.
changedR :: (ArrowPlus (~>), ArrowApply (~>), Eq a) => (a ~> a) -> (a ~> a)
changedR r = readerR (\ a -> r >>> acceptR (/=a))

-- | repeat an 'ArrowPlus' until it fails, then return the result before the failure.
--   requires at least the first attempt to succeed.
repeatR :: ArrowPlus (~>) => (a ~> a) -> (a ~> a)
repeatR r = r >>> tryR (repeatR r)

-- | attempts two 'Arrows's in sequence, succeeding if one or both succeed.
(>+>) :: (ArrowPlus (~>), ArrowApply (~>)) => (a ~> a) -> (a ~> a) -> (a ~> a)
r1 >+> r2 = attemptR r1 >>> readerR (\ (b,_) -> snd ^>> if b then tryR r2 else r2)

-- | attempt a list of 'Arrow's in sequence, succeeding if any succeed.
orR :: (ArrowZero (~>), ArrowPlus (~>), ArrowApply (~>)) => [a ~> a] -> (a ~> a)
orR = foldl (>+>) zeroArrow

-- | sequence a list of 'Arrow's, succeeding if they all succeed.
andR :: Arrow (~>) => [a ~> a] -> (a ~> a)
andR = foldl (>>>) id

-------------------------------------------------------------------------------
