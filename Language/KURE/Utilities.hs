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
         KureMonad
       , runKureMonad
         -- * Error Messages
       , missingChild
         -- * Generic Combinators
         -- $genericdoc
       , allTgeneric
       , allRgeneric
       , anyRgeneric
       , childLgeneric
         -- * Attempt Combinators
         -- $attemptdoc
       , attemptAny2
       , attemptAny3
       , attemptAny4
       , attemptAnyN
       , attemptAny1N
         -- * Child Combinators
         -- $childLdoc
       , childLaux
       , childL0of1
       , childL0of2
       , childL1of2
       , childL0of3
       , childL1of3
       , childL2of3
       , childL0of4
       , childL1of4
       , childL2of4
       , childL3of4
       , childLMofN
) where

import Control.Applicative
import Control.Monad
import Control.Arrow

import Data.Monoid

import Language.KURE.Combinators
import Language.KURE.Translate
import Language.KURE.Walker
import Language.KURE.Injection

-------------------------------------------------------------------------------

-- | A basic error 'Monad'.  KURE users may use either 'KureMonad' or their own 'Monad'(s).
data KureMonad a = Failure String | Success a deriving (Eq, Show)

-- | Eliminator for 'KureMonad'.
runKureMonad :: (a -> b) -> (String -> b) -> KureMonad a -> b
runKureMonad _ f (Failure msg) = f msg
runKureMonad s _ (Success a)   = s a

instance Monad KureMonad where
-- return :: a -> KureMonad a
   return = Success

-- (>>=) :: KureMonad a -> (a -> KureMonad b) -> KureMonad b
   (Success a)   >>= f = f a
   (Failure msg) >>= _ = Failure msg

-- fail :: String -> KureMonad a
   fail = Failure

-- | 'KureMonad' is the minimal monad that can be an instance of 'MonadCatch'.
instance MonadCatch KureMonad where
-- catchM :: KureMonad a -> (a -> KureMonad b) -> KureMonad b
   (Success a)   `catchM` _ = Success a
   (Failure msg) `catchM` f = f msg

instance Functor KureMonad where
-- fmap :: (a -> b) -> KureMonad a -> KureMonad b
   fmap = liftM

instance Applicative KureMonad where
-- pure :: a -> KureMonad a
   pure = return

-- (<*>) :: KureMonad (a -> b) -> KureMonad a -> KureMonad b
   (<*>) = ap

------------------------------------------------------------------------------------------

-- $genericdoc
-- These functions are to aid with defining 'Walker' instances for the 'Generic' type.
-- See the \"Expr\" example.

allTgeneric :: (Walker c m a, Monoid b) => Translate c m (Generic a) b -> c -> a -> m b
allTgeneric t c a = inject `liftM` apply (allT t) c a

allRgeneric :: Walker c m a => Rewrite c m (Generic a) -> c -> a -> m (Generic a)
allRgeneric r c a = inject `liftM` apply (allR r) c a

anyRgeneric :: Walker c m a => Rewrite c m (Generic a) -> c -> a -> m (Generic a)
anyRgeneric r c a = inject `liftM` apply (anyR r) c a

childLgeneric :: Walker c m a => Int -> c -> a -> m ((c, Generic a), Generic a -> m (Generic a))
childLgeneric n c a = (liftM.second.result.liftM) inject $ apply (lensT $ childL n) c a

-------------------------------------------------------------------------------

-- $attemptdoc
-- These are useful when defining congruence combinators that succeed if any child rewrite succeeds.
-- As well as being generally useful, such combinators are helpful when defining 'anyR' instances.
-- See the \"Lam\" or \"Expr\" examples, or the HERMIT package.

attemptAny2 :: Monad m => (a1 -> a2 -> r) -> m (Bool,a1) -> m (Bool,a2) -> m r
attemptAny2 f mba1 mba2 = do (b1,a1) <- mba1
                             (b2,a2) <- mba2
                             if b1 || b2
                              then return (f a1 a2)
                              else fail "failed for both children"

attemptAny3 :: Monad m => (a1 -> a2 -> a3 -> r) -> m (Bool,a1) -> m (Bool,a2) -> m (Bool,a3) -> m r
attemptAny3 f mba1 mba2 mba3 = do (b1,a1) <- mba1
                                  (b2,a2) <- mba2
                                  (b3,a3) <- mba3
                                  if b1 || b2 || b3
                                   then return (f a1 a2 a3)
                                   else fail "failed for all three children"

attemptAny4 :: Monad m => (a1 -> a2 -> a3 -> a4 -> r) -> m (Bool,a1) -> m (Bool,a2) -> m (Bool,a3) -> m (Bool,a4) -> m r
attemptAny4 f mba1 mba2 mba3 mba4 = do (b1,a1) <- mba1
                                       (b2,a2) <- mba2
                                       (b3,a3) <- mba3
                                       (b4,a4) <- mba4
                                       if b1 || b2 || b3 || b4
                                        then return (f a1 a2 a3 a4)
                                        else fail "failed for all four children"

attemptAnyN :: Monad m => ([a] -> b) -> [m (Bool,a)] -> m b
attemptAnyN f mbas = do (bs,as) <- unzip `liftM` sequence mbas
                        if or bs
                         then return (f as)
                         else fail ("failed for all " ++ show (length bs) ++ " children")

attemptAny1N :: Monad m => (a1 -> [a2] -> r) -> m (Bool,a1) -> [m (Bool,a2)] -> m r
attemptAny1N f mba mbas = do (b ,a)  <- mba
                             (bs,as) <- unzip `liftM` sequence mbas
                             if or (b:bs)
                               then return (f a as)
                               else fail ("failed for all " ++ show (1 + length bs) ++ " children")

-------------------------------------------------------------------------------

-- | A standard error message for when the child index is out of bounds.

missingChild :: Int -> String
missingChild n = "there is no child number " ++ show n

-------------------------------------------------------------------------------

-- $childLdoc
-- These functions are helpful when defining 'childL' instances in combination with congruence combinators.
-- See the \"Lam\" and \"Expr\" examples, or the HERMIT package.
--
-- Unfortunately they increase quadratically with the number of fields of the constructor.
-- It would be nice if they were further expanded to include the calls of 'id' and 'exposeT';
-- however this would create a plethora of additional cases as the number (and positions)
-- of interesting children would be additional dimensions.
--
-- Note that the numbering scheme MofN is that N is the number of children (including uninteresting children)
-- and M is the index of the chosen child, starting with index 0.  Thus M ranges from 0 to (n-1).
--
-- TO DO: use Template Haskell to generate these.
--
-- In the mean time, if you need a few more than provided here, drop me an email and I'll add them.

childLaux :: (MonadCatch m, Node b) => (c,b) -> (b -> a) -> ((c, Generic b), Generic b -> m a)
childLaux cb g = (second inject cb, liftM (inject.g) . retractM)

childL0of1 :: (MonadCatch m, Node b) => (b -> a) -> (c,b) -> ((c, Generic b) , Generic b -> m a)
childL0of1 f cb = childLaux cb f

childL0of2 :: (MonadCatch m, Node b0) => (b0 -> b1 -> a) -> (c,b0) -> b1 -> ((c, Generic b0) , Generic b0 -> m a)
childL0of2 f cb0 b1 = childLaux cb0 (\ b0 -> f b0 b1)

childL1of2 :: (MonadCatch m, Node b1) => (b0 -> b1 -> a) -> b0 -> (c,b1) -> ((c, Generic b1) , Generic b1 -> m a)
childL1of2 f b0 cb1 = childLaux cb1 (\ b1 -> f b0 b1)

childL0of3 :: (MonadCatch m, Node b0) => (b0 -> b1 -> b2 -> a) -> (c,b0) -> b1 -> b2 -> ((c, Generic b0) , Generic b0 -> m a)
childL0of3 f cb0 b1 b2 = childLaux cb0 (\ b0 -> f b0 b1 b2)

childL1of3 :: (MonadCatch m, Node b1) => (b0 -> b1 -> b2 -> a) -> b0 -> (c,b1) -> b2 -> ((c, Generic b1) , Generic b1 -> m a)
childL1of3 f b0 cb1 b2 = childLaux cb1 (\ b1 -> f b0 b1 b2)

childL2of3 :: (MonadCatch m, Node b2) => (b0 -> b1 -> b2 -> a) -> b0 -> b1 -> (c,b2) -> ((c, Generic b2) , Generic b2 -> m a)
childL2of3 f b0 b1 cb2 = childLaux cb2 (\ b2 -> f b0 b1 b2)

childL0of4 :: (MonadCatch m, Node b0) => (b0 -> b1 -> b2 -> b3 -> a) -> (c,b0) -> b1 -> b2 -> b3 -> ((c, Generic b0) , Generic b0 -> m a)
childL0of4 f cb0 b1 b2 b3 = childLaux cb0 (\ b0 -> f b0 b1 b2 b3)

childL1of4 :: (MonadCatch m, Node b1) => (b0 -> b1 -> b2 -> b3 -> a) -> b0 -> (c,b1) -> b2 -> b3 -> ((c, Generic b1) , Generic b1 -> m a)
childL1of4 f b0 cb1 b2 b3 = childLaux cb1 (\ b1 -> f b0 b1 b2 b3)

childL2of4 :: (MonadCatch m, Node b2) => (b0 -> b1 -> b2 -> b3 -> a) -> b0 -> b1 -> (c,b2) -> b3 -> ((c, Generic b2) , Generic b2 -> m a)
childL2of4 f b0 b1 cb2 b3 = childLaux cb2 (\ b2 -> f b0 b1 b2 b3)

childL3of4 :: (MonadCatch m, Node b3) => (b0 -> b1 -> b2 -> b3 -> a) -> b0 -> b1 -> b2 -> (c,b3) -> ((c, Generic b3) , Generic b3 -> m a)
childL3of4 f b0 b1 b2 cb3 = childLaux cb3 (\ b3 -> f b0 b1 b2 b3)

childLMofN :: (MonadCatch m, Node b) => Int -> ([b] -> a) -> [(c,b)] -> ((c, Generic b) , Generic b -> m a)
childLMofN m f cbs = childLaux (cbs !! m) (\ b' -> f $ atIndex m (const b') (map snd cbs))

-------------------------------------------------------------------------------

-- | Modify the value in a list at specified index.
atIndex :: Int -> (a -> a) -> [a] -> [a]
atIndex i f as = [ if n == i then f a else a
                 | (a,n) <- zip as [0..]
                 ]

-------------------------------------------------------------------------------
