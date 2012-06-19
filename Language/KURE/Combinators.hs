{-# LANGUAGE TypeOperators, TupleSections #-}

-- |
-- Module: Language.KURE.Combinators
-- Copyright: (c) 2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
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
             -- ** Categories with a Catch
           , CategoryCatch(..)
             -- ** Combinators
           , idR
           , tagFailR
           , readerR
           , acceptR
           , tryR
           , attemptR
           , changedR
           , repeatR
           , (>+>)
           , orR
           , andR
             -- ** Basic Routing
             -- | The names 'result' and 'argument' are taken from Conal Elliott's semantic editor combinators.
           , result
           , argument
           , fstR
           , sndR
           , toFst
           , toSnd
           , swap
           , fork
           , forkFirst
           , forkSecond
) where

import Prelude hiding (id , (.))
import Control.Monad
import Control.Category
import Control.Arrow
import Data.Maybe (isJust)
import Data.Monoid

infixl 3 >+>

------------------------------------------------------------------------------------------

-- | Similar to 'guard', but using 'fail' rather than 'mzero'.
guardFail ::  Monad m => Bool -> String -> m ()
guardFail b msg = unless b (fail msg)

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

-- | Categories with failure and a catch.
class Category (~>) => CategoryCatch (~>) where

-- | The failing arrow.
  failR :: String -> a ~> b

-- | A catch on arrows.
  (<+) :: (a ~> b) -> (a ~> b) -> (a ~> b)


-- | Synonym for 'id'.
idR :: Category (~>) => (a ~> a)
idR = id

-- | Replace the error message of a failing 'CategoryCatch'.
--   Successful 'CategoryCatch'es are unaffected.
tagFailR :: CategoryCatch (~>) => String -> (a ~> b) -> (a ~> b)
tagFailR msg r = r <+ failR msg

-- | Look at the argument to the 'Arrow' before choosing which 'Arrow' to use.
readerR :: ArrowApply (~>) => (a -> (a ~> b)) -> (a ~> b)
readerR f = (f &&& id) ^>> app

-- | Look at the argument to an 'Arrow', and choose to be either the identity arrow or the zero arrow.
acceptR :: (CategoryCatch (~>), ArrowApply (~>)) => (a -> Bool) -> (a ~> a)
acceptR p = readerR $ \ a -> if p a then id else failR "acceptR: predicate failed"

-- | Catch a failing 'ArrowPlus', making it into an identity.
tryR :: CategoryCatch (~>) => (a ~> a) -> (a ~> a)
tryR r = r <+ id

-- | Catch a failing 'ArrowPlus', making it succeed with a Boolean flag.
--   Useful when defining 'anyR' instances.
attemptR :: (CategoryCatch (~>), Arrow (~>)) => (a ~> a) -> (a ~> (Bool,a))
attemptR r = (r >>^ (True,)) <+ arr (False,)

-- | Makes an 'Arrow' fail if the result value equals the argument value.
changedR :: (CategoryCatch (~>), ArrowApply (~>), Eq a) => (a ~> a) -> (a ~> a)
changedR r = readerR (\ a -> r >>> acceptR (/=a))

-- | Repeat an 'ArrowPlus' until it fails, then return the result before the failure.
--   Requires at least the first attempt to succeed.
repeatR :: CategoryCatch (~>) => (a ~> a) -> (a ~> a)
repeatR r = r >>> tryR (repeatR r)

-- | Attempts two 'Arrow's in sequence, succeeding if one or both succeed.
(>+>) :: (CategoryCatch (~>), ArrowApply (~>)) => (a ~> a) -> (a ~> a) -> (a ~> a)
r1 >+> r2 = attemptR r1 >>> readerR (\ (b,_) -> snd ^>> if b then tryR r2 else r2)

-- | Sequence a list of 'Arrow's, succeeding if any succeed.
orR :: (CategoryCatch (~>), ArrowApply (~>)) => [a ~> a] -> (a ~> a)
orR = foldl (>+>) (failR "orR failed")

-- | Sequence a list of 'Arrow's, succeeding if they all succeed.
andR :: Category (~>) => [a ~> a] -> (a ~> a)
andR = foldl (>>>) id

------------------------------------------------------------------------------------------

-- | Apply a pure function to the result of an 'Arrow'.
result :: Arrow (~>) => (b -> c) -> (a ~> b) -> (a ~> c)
result f a = a >>^ f

-- | Apply a pure function to the argument to an 'Arrow'.
argument :: Arrow (~>) => (a -> b) -> (b ~> c) -> (a ~> c)
argument f a = f ^>> a

-- | A pure 'Arrow' that projects the first element of a pair.
fstR :: Arrow (~>) => ((a,b) ~> a)
fstR = arr fst

-- | A pure 'Arrow' that projects the second element of a pair.
sndR :: Arrow (~>) => ((a,b) ~> b)
sndR = arr snd

-- | Apply an 'Arrow' to the first element of a pair, discarding the second element.
toFst :: Arrow (~>) => (a ~> b) -> ((a,x) ~> b)
toFst f = fstR >>> f

-- | Apply an 'Arrow' to the second element of a pair, discarding the first element.
toSnd :: Arrow (~>) => (a ~> b) -> ((x,a) ~> b)
toSnd f = sndR >>> f

-- | A pure 'Arrow' that swaps the elements of a pair.
swap :: Arrow (~>) => ((a,b) ~> (b,a))
swap = arr (\(a,b) -> (b,a))

-- | A pure 'Arrow' that duplicates its argument.
fork :: Arrow (~>) => (a ~> (a,a))
fork = arr (\a -> (a,a))

-- | Tag the result of an 'Arrow' with its argument.
forkFirst :: Arrow (~>) => (a ~> b) -> (a ~> (b , a))
forkFirst sf = fork >>> first sf

-- | Tag the result of an 'Arrow' with its argument.
forkSecond :: Arrow (~>) => (a ~> b) -> (a ~> (a , b))
forkSecond sf = fork >>> second sf

-------------------------------------------------------------------------------
