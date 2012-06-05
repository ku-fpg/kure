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
-- This module provides various monadic and arrow combinators that are particularly useful when
-- working with translations.
-- Note that these combinators assume that 'mplus' behaves as a catch, for both 'fail' and 'mzero'.

module Language.KURE.Combinators
           ( -- * Monad Combinators
             guardFail
           , condM
           , whenM
           , tryM
           , mtryM
           , attemptM
           , testM
           , notM
             -- * Arrow Combinators
             -- | The names 'result' and 'argument' are taken from Conal Elliott's semantic editor combinators.
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
import Data.Maybe (isJust)
import Data.Monoid

infixl 3 <+, >+>

------------------------------------------------------------------------------------------

-- | Similar to 'guard', but using 'fail' rather than 'mzero'.
guardFail ::  Monad m => Bool -> String -> m ()
guardFail b msg = if b then return () else fail msg

-- | if-then-else lifted over a 'Monad'.
condM ::  Monad m => m Bool -> m a -> m a -> m a
condM mb m1 m2 = do b <- mb
                    if b then m1 else m2

-- | if-then lifted over a 'Monad'.
whenM ::  Monad m => m Bool -> m a -> m a
whenM mb ma = condM mb ma (fail "condition False")

-- | Catch a failing monadic computation, making it succeed with a constant value.
tryM :: MonadPlus m => a -> m a -> m a
tryM a ma = ma `mplus` return a

-- | Catch a failing monadic computation, making it succeed with 'mempty'.
mtryM :: (MonadPlus m, Monoid a) => m a -> m a
mtryM = tryM mempty

-- | Catch a failing monadic computation, making it succeed with 'Nothing'.
attemptM :: MonadPlus m => m a -> m (Maybe a)
attemptM = tryM Nothing . liftM Just

-- | Determine if a monadic computation succeeds.
testM :: MonadPlus m => m a -> m Bool
testM = liftM isJust . attemptM

-- | Fail if the 'Monad' succeeds; succeed with @()@ if it fails.
notM :: MonadPlus m => m a -> m ()
notM ma = attemptM ma >>= maybe (return ()) (const mzero)

------------------------------------------------------------------------------------------

-- | Apply a pure function to the result of an 'Arrow'.
result :: Arrow (~>) => (b -> c) -> (a ~> b) -> (a ~> c)
result f a = a >>^ f

-- | Apply a pure function to the argument to an 'Arrow'.
argument :: Arrow (~>) => (a -> b) -> (b ~> c) -> (a ~> c)
argument f a = f ^>> a

-------------------------------------------------------------------------------

-- | Synonym for 'id'.
idR :: Category (~>) => (a ~> a)
idR = id

-- | Synonym for '<+>'.
(<+) :: ArrowPlus (~>) => (a ~> b) -> (a ~> b) -> (a ~> b)
(<+) = (<+>)

-- | Look at the argument to the 'Arrow' before choosing which 'Arrow' to use.
readerR :: ArrowApply (~>) => (a -> (a ~> b)) -> (a ~> b)
readerR f = (f &&& id) ^>> app

-- | Look at the argument to an 'Arrow', and choose to be either the identity arrow or the zero arrow.
acceptR :: (ArrowZero (~>), ArrowApply (~>)) => (a -> Bool) -> (a ~> a)
acceptR p = readerR $ \ a -> if p a then id else zeroArrow

-- | Catch a failing 'ArrowPlus', making it into an identity.
tryR :: ArrowPlus (~>) => (a ~> a) -> (a ~> a)
tryR r = r <+> id

-- | Catch a failing 'ArrowPlus', making it succeed with a Boolean flag.
--   Useful when defining 'anyR' instances.
attemptR :: ArrowPlus (~>) => (a ~> a) -> (a ~> (Bool,a))
attemptR r = (r >>^ (True,)) <+> arr (False,)

-- | Makes an 'Arrow' fail if the result value equals the argument value.
changedR :: (ArrowPlus (~>), ArrowApply (~>), Eq a) => (a ~> a) -> (a ~> a)
changedR r = readerR (\ a -> r >>> acceptR (/=a))

-- | Repeat an 'ArrowPlus' until it fails, then return the result before the failure.
--   Requires at least the first attempt to succeed.
repeatR :: ArrowPlus (~>) => (a ~> a) -> (a ~> a)
repeatR r = r >>> tryR (repeatR r)

-- | Attempts two 'Arrows's in sequence, succeeding if one or both succeed.
(>+>) :: (ArrowPlus (~>), ArrowApply (~>)) => (a ~> a) -> (a ~> a) -> (a ~> a)
r1 >+> r2 = attemptR r1 >>> readerR (\ (b,_) -> snd ^>> if b then tryR r2 else r2)

-- | Sequence a list of 'Arrow's, succeeding if any succeed.
orR :: (ArrowZero (~>), ArrowPlus (~>), ArrowApply (~>)) => [a ~> a] -> (a ~> a)
orR = foldl (>+>) zeroArrow

-- | Sequence a list of 'Arrow's, succeeding if they all succeed.
andR :: Arrow (~>) => [a ~> a] -> (a ~> a)
andR = foldl (>>>) id

-------------------------------------------------------------------------------
