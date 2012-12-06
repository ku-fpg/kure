{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

-- |
-- Module: Language.KURE.Utilities
-- Copyright: (c) 2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module contains various utilities that can be useful to users of KURE, but are not essential.

module Language.KURE.Utilities
       ( -- * The KURE Monad
         KureM
       , runKureM
       , fromKureM
         -- * Error Messages
       , missingChild
         -- * Attempt Combinators
         -- ** anyR Support
         -- $anyR_helperdoc
       , anyR_helper2
       , anyR_helper3
       , anyR_helper4
       , anyR_helper
       , anyR_helper1N
         -- * oneR Support
         -- $oneR_helperdoc
       -- , withArgumentT
       , oneR_helper2
       , oneR_helper3
       , oneR_helper4
       , oneR_helper
       , oneR_helper1N
) where

import Prelude hiding (sequence, mapM, or, foldr)

import Control.Applicative
import Control.Monad hiding (sequence, mapM)
import Control.Arrow

import Data.Foldable
import Data.Traversable

import Language.KURE.Combinators

-------------------------------------------------------------------------------

-- | 'KureM' is a basic error 'Monad'.
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

-- | 'KureM' is the minimal monad that can be an instance of 'MonadCatch'.
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

-- $anyR_helperdoc
-- These are useful when defining congruence combinators that succeed if /any/ child rewrite succeeds.
-- See the \"Expr\" example, or the HERMIT package.

anyR_helper2' :: Monad m => (a1 -> a2 -> r) -> (Bool,a1) -> (Bool,a2) -> m r
anyR_helper2' f (b1,a1) (b2,a2) = if b1 || b2
                                  then return (f a1 a2)
                                  else fail "failed for both children"
{-# INLINE anyR_helper2' #-}

anyR_helper3' :: Monad m => (a1 -> a2 -> a3 -> r) -> (Bool,a1) -> (Bool,a2) -> (Bool,a3) -> m r
anyR_helper3' f (b1,a1) (b2,a2) (b3,a3) = if b1 || b2 || b3
                                          then return (f a1 a2 a3)
                                          else fail "failed for all three children"
{-# INLINE anyR_helper3' #-}

anyR_helper4' :: Monad m => (a1 -> a2 -> a3 -> a4 -> r) -> (Bool,a1) -> (Bool,a2) -> (Bool,a3) -> (Bool,a4) -> m r
anyR_helper4' f (b1,a1) (b2,a2) (b3,a3) (b4,a4) = if b1 || b2 || b3 || b4
                                                  then return (f a1 a2 a3 a4)
                                                  else fail "failed for all four children"
{-# INLINE anyR_helper4' #-}

anyR_helper' :: (Traversable t, Monad m) => (t a -> b) -> t (Bool,a) -> m b
anyR_helper' f bas = let (bs,as) = (fmap fst &&& fmap snd) $ bas
                      in if or bs
                          then return (f as)
                          else fail ("failed for all " ++ show (length $ toList bs) ++ " children")
{-# INLINE anyR_helper' #-}

anyR_helper1N' :: (Traversable t, Monad m) => (a1 -> t a2 -> r) -> (Bool,a1) -> t (Bool,a2) -> m r
anyR_helper1N' f (b,a) bas = let (bs,as) = (fmap fst &&& fmap snd) $ bas
                             in if b || or bs
                                 then return (f a as)
                                 else fail ("failed for all " ++ show (1 + length (toList bs)) ++ " children")
{-# INLINE anyR_helper1N' #-}

anyR_helper2 :: Monad m => (a1 -> a2 -> r) -> m (Bool,a1) -> m (Bool,a2) -> m r
anyR_helper2 f = liftArgument2 (anyR_helper2' f)
{-# INLINE anyR_helper2 #-}

anyR_helper3 :: Monad m => (a1 -> a2 -> a3 -> r) -> m (Bool,a1) -> m (Bool,a2) -> m (Bool,a3) -> m r
anyR_helper3 f = liftArgument3 (anyR_helper3' f)
{-# INLINE anyR_helper3 #-}

anyR_helper4 :: Monad m => (a1 -> a2 -> a3 -> a4 -> r) -> m (Bool,a1) -> m (Bool,a2) -> m (Bool,a3) -> m (Bool,a4) -> m r
anyR_helper4 f = liftArgument4 (anyR_helper4' f)
{-# INLINE anyR_helper4 #-}

anyR_helper :: (Traversable t, Monad m) => (t a -> b) -> t (m (Bool,a)) -> m b
anyR_helper f = liftArgumentN (anyR_helper' f)
{-# INLINE anyR_helper #-}

anyR_helper1N :: (Traversable t, Monad m) => (a1 -> t a2 -> r) -> m (Bool,a1) -> t (m (Bool,a2)) -> m r
anyR_helper1N f = liftArgument1N (anyR_helper1N' f)
{-# INLINE anyR_helper1N #-}

-------------------------------------------------------------------------------

-- $oneR_helperdoc
-- These are useful when defining congruence combinators that succeed if one child rewrite succeeds
-- (and the remainder are then discarded).
-- As well as being generally useful, such combinators are helpful when defining 'oneR' instances.
-- See the \"Expr\" example, or the HERMIT package.

oneR_helper1' :: Monad m => (a -> r) -> (m a, a) -> m r
oneR_helper1' f (ma , _) = f `liftM` ma
{-# INLINE oneR_helper1' #-}

oneR_helper2' :: MonadCatch m => (a -> b -> r) -> (m a, a) -> (m b, b) -> m r
oneR_helper2' f (ma , a) mbb@(_ , b) = (<<+ oneR_helper1' (f a) mbb) $
                                       do a' <- ma
                                          return (f a' b)
{-# INLINE oneR_helper2' #-}

oneR_helper3' :: MonadCatch m => (a -> b -> c -> r) -> (m a, a) -> (m b, b) -> (m c, c) -> m r
oneR_helper3' f (ma , a) mbb@(_ , b) mcc@(_ , c) = (do a' <- ma
                                                       return (f a' b c)
                                                  ) <<+ oneR_helper2' (f a) mbb mcc
{-# INLINE oneR_helper3' #-}

oneR_helper4' :: MonadCatch m => (a -> b -> c -> d -> r) -> (m a, a) -> (m b, b) -> (m c, c) -> (m d, d) -> m r
oneR_helper4' f (ma , a) mbb@(_ , b) mcc@(_ , c) mdd@(_ , d) = (do a' <- ma
                                                                   return (f a' b c d)
                                                              ) <<+ oneR_helper3' (f a) mbb mcc mdd
{-# INLINE oneR_helper4' #-}

oneR_helper2 :: MonadCatch m => (a -> b -> r) -> m (m a, a) -> m (m b, b) -> m r
oneR_helper2 f = liftArgument2 (oneR_helper2' f)
{-# INLINE oneR_helper2 #-}

oneR_helper3 :: MonadCatch m => (a -> b -> c -> r) -> m (m a, a) -> m (m b, b) -> m (m c, c) -> m r
oneR_helper3 f = liftArgument3 (oneR_helper3' f)
{-# INLINE oneR_helper3 #-}

oneR_helper4 :: MonadCatch m => (a -> b -> c -> d -> r) -> m (m a, a) -> m (m b, b) -> m (m c, c) -> m (m d, d) -> m r
oneR_helper4 f = liftArgument4 (oneR_helper4' f)
{-# INLINE oneR_helper4 #-}


newtype S s m a = S {runS :: s -> m (a, s)}

instance Monad m => Functor (S s m) where
  fmap = liftM
  {-# INLINE fmap #-}

instance Monad m => Applicative (S s m) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = liftM2 ($)

instance Monad m => Monad (S s m) where
  {-# INLINE return #-}
  return a = S $ \ b -> return (a, b)
  {-# INLINE (>>=) #-}
  m >>= f = S $ \ b -> runS m b >>= \(a, b') -> runS (f a) b'

{-# INLINE oneR_helper #-}
oneR_helper :: forall t m a r. (Traversable t, MonadCatch m) => (t a -> r) -> t (m (m a, a)) -> m r
oneR_helper f tmmaa = runS (mapM each tmmaa) False >>= final
  where
    {-# INLINE each #-}
    each :: m (m a, a) -> S Bool m a
    each mmaa = S $ \ b -> do (ma, a) <- mmaa
                              if b
                                then return (a, b)
                                else liftM (,True) ma <<+ return (a, b)
    {-# INLINE final #-}
    final :: (t a, Bool) -> m r
    final (ta, b) = if b
                      then return (f ta)
                      else fail "failed for all children."

oneR_helper1N :: (Traversable t, MonadCatch m) => (a -> t b -> r) -> m (m a, a) -> t (m (m b, b)) -> m r
oneR_helper1N f mmaa mmbbs = do
  (ma, a) <- mmaa
  mbbs    <- sequence mmbbs
  ((\a' -> f a' $ fmap snd mbbs) `liftM` ma) <<+ oneR_helper (f a) (return `fmap` mbbs)
{-# INLINE oneR_helper1N #-}

-------------------------------------------------------------------------------

-- | A standard error message for when the child index is out of bounds.
missingChild :: Int -> String
missingChild n = "there is no child number " ++ show n
{-# INLINE missingChild #-}

-------------------------------------------------------------------------------

liftArgument2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftArgument2 f ma mb = join (liftM2 f ma mb)
{-# INLINE liftArgument2 #-}

liftArgument3 :: Monad m => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
liftArgument3 f ma mb mc = join (liftM3 f ma mb mc)
{-# INLINE liftArgument3 #-}

liftArgument4 :: Monad m => (a -> b -> c -> d -> m e) -> m a -> m b -> m c -> m d -> m e
liftArgument4 f ma mb mc md = join (liftM4 f ma mb mc md)
{-# INLINE liftArgument4 #-}

liftArgumentN :: (Traversable t, Monad m) => (t a -> m b) -> t (m a) -> m b
liftArgumentN f mas = sequence mas >>= f
{-# INLINE liftArgumentN #-}

liftArgument1N :: (Traversable t, Monad m) => (a -> t b -> m c) -> m a -> t (m b) -> m c
liftArgument1N f ma mbs = do a  <- ma
                             bs <- sequence mbs
                             f a bs
{-# INLINE liftArgument1N #-}

-------------------------------------------------------------------------------
