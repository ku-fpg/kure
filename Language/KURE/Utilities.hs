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
         -- * Generic Combinators
         -- $genericdoc
       , allTgeneric
       , oneTgeneric
       , allRgeneric
       , anyRgeneric
       , oneRgeneric
       , childLgeneric
         -- * Attempt Combinators
         -- ** anyR Support
         -- $attemptAnydoc
       , attemptAny2
       , attemptAny3
       , attemptAny4
       , attemptAnyN
       , attemptAny1N
         -- * oneR Support
         -- $attemptOnedoc
       , withArgumentT
       , attemptOne2
       , attemptOne3
       , attemptOne4
       , attemptOneN
       , attemptOne1N
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

import Data.Traversable (Traversable)
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable

import Language.KURE.Combinators
import Language.KURE.Translate
import Language.KURE.Walker
import Language.KURE.Injection

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

-- | Get the value from a 'KureM', providing a function to handle the error case.
fromKureM :: (String -> a) -> KureM a -> a
fromKureM = runKureM id

instance Monad KureM where
-- return :: a -> KureM a
   return = Success

-- (>>=) :: KureM a -> (a -> KureM b) -> KureM b
   (Success a)   >>= f = f a
   (Failure msg) >>= _ = Failure msg

-- fail :: String -> KureM a
   fail = Failure

-- | 'KureM' is the minimal monad that can be an instance of 'MonadCatch'.
instance MonadCatch KureM where
-- catchM :: KureM a -> (String -> KureM a) -> KureM a
   (Success a)   `catchM` _ = Success a
   (Failure msg) `catchM` f = f msg

instance Functor KureM where
-- fmap :: (a -> b) -> KureM a -> KureM b
   fmap = liftM

instance Applicative KureM where
-- pure :: a -> KureM a
   pure = return

-- (<*>) :: KureM (a -> b) -> KureM a -> KureM b
   (<*>) = ap

------------------------------------------------------------------------------------------

-- $genericdoc
-- These functions are to aid with defining 'Walker' instances for the 'Generic' type.
-- See the \"Expr\" example.

allTgeneric :: (Walker c m a, Monoid b) => Translate c m (Generic a) b -> c -> a -> m b
allTgeneric t c a = inject `liftM` apply (allT t) c a

oneTgeneric :: Walker c m a => Translate c m (Generic a) b -> c -> a -> m b
oneTgeneric t c a = inject `liftM` apply (oneT t) c a

allRgeneric :: Walker c m a => Rewrite c m (Generic a) -> c -> a -> m (Generic a)
allRgeneric r c a = inject `liftM` apply (allR r) c a

anyRgeneric :: Walker c m a => Rewrite c m (Generic a) -> c -> a -> m (Generic a)
anyRgeneric r c a = inject `liftM` apply (anyR r) c a

oneRgeneric :: Walker c m a => Rewrite c m (Generic a) -> c -> a -> m (Generic a)
oneRgeneric r c a = inject `liftM` apply (oneR r) c a

childLgeneric :: Walker c m a => Int -> c -> a -> m ((c, Generic a), Generic a -> m (Generic a))
childLgeneric n c a = (liftM.second.result.liftM) inject $ apply (lensT $ childL n) c a

-------------------------------------------------------------------------------

-- $attemptAnydoc
-- These are useful when defining congruence combinators that succeed if any child rewrite succeeds.
-- As well as being generally useful, such combinators are helpful when defining 'anyR' instances.
-- See the \"Expr\" example, or the HERMIT package.

attemptAny2' :: Monad m => (a1 -> a2 -> r) -> (Bool,a1) -> (Bool,a2) -> m r
attemptAny2' f (b1,a1) (b2,a2) = if b1 || b2
                                  then return (f a1 a2)
                                  else fail "failed for both children"

attemptAny3' :: Monad m => (a1 -> a2 -> a3 -> r) -> (Bool,a1) -> (Bool,a2) -> (Bool,a3) -> m r
attemptAny3' f (b1,a1) (b2,a2) (b3,a3) = if b1 || b2 || b3
                                          then return (f a1 a2 a3)
                                          else fail "failed for all three children"

attemptAny4' :: Monad m => (a1 -> a2 -> a3 -> a4 -> r) -> (Bool,a1) -> (Bool,a2) -> (Bool,a3) -> (Bool,a4) -> m r
attemptAny4' f (b1,a1) (b2,a2) (b3,a3) (b4,a4) = if b1 || b2 || b3 || b4
                                                  then return (f a1 a2 a3 a4)
                                                  else fail "failed for all four children"

attemptAnyN' :: (Traversable t, Monad m) => (t a -> b) -> t (Bool,a) -> m b
attemptAnyN' f bas = let (bs,as) = fmap fst &&& fmap snd $ bas
                      in if Foldable.or bs
                          then return (f as)
                          else fail ("failed for all " ++ show (length $ Foldable.toList bs) ++ " children")

attemptAny1N' :: (Traversable t, Monad m) => (a1 -> t a2 -> r) -> (Bool,a1) -> t (Bool,a2) -> m r
attemptAny1N' f (b,a) bas = let (bs,as) = fmap fst &&& fmap snd $ bas
                             in if b || Foldable.or bs
                                 then return (f a as)
                                 else fail ("failed for all " ++ show (1 + length (Foldable.toList bs)) ++ " children")

attemptAny2 :: Monad m => (a1 -> a2 -> r) -> m (Bool,a1) -> m (Bool,a2) -> m r
attemptAny2 f = liftArgument2 (attemptAny2' f)

attemptAny3 :: Monad m => (a1 -> a2 -> a3 -> r) -> m (Bool,a1) -> m (Bool,a2) -> m (Bool,a3) -> m r
attemptAny3 f = liftArgument3 (attemptAny3' f)

attemptAny4 :: Monad m => (a1 -> a2 -> a3 -> a4 -> r) -> m (Bool,a1) -> m (Bool,a2) -> m (Bool,a3) -> m (Bool,a4) -> m r
attemptAny4 f = liftArgument4 (attemptAny4' f)

attemptAnyN :: (Traversable t, Monad m) => (t a -> b) -> t (m (Bool,a)) -> m b
attemptAnyN f = liftArgumentN (attemptAnyN' f)

attemptAny1N :: (Traversable t, Monad m) => (a1 -> t a2 -> r) -> m (Bool,a1) -> t (m (Bool,a2)) -> m r
attemptAny1N f = liftArgument1N (attemptAny1N' f)

-------------------------------------------------------------------------------

-- -- | Catch a failing 'Arrow', making it succeed with an error message, and passing through the argument value.
-- --   Useful when defining 'Language.KURE.Walker.oneR' instances.
-- attemptT :: MonadCatch m => Translate c m a b -> Translate c m a (Either String b, a)
-- attemptT t = forkFirst (attemptM t)

-- $attemptOnedoc
-- These are useful when defining congruence combinators that succeed if one child rewrite succeeds
-- (and the remainder are then discarded).
-- As well as being generally useful, such combinators are helpful when defining 'oneR' instances.
-- See the \"Expr\" example, or the HERMIT package.

-- | Return the monadic result of a 'Translate' and pair it with the argument.
withArgumentT :: Monad m => Translate c m a b -> Translate c m a (m b, a)
withArgumentT t = do (c,a) <- exposeT
                     return (apply t c a, a)

attemptOne1' :: Monad m => (a -> r) -> (m a, a) -> m r
attemptOne1' f (ma , _) = f `liftM` ma

attemptOne2' :: MonadCatch m => (a -> b -> r) -> (m a, a) -> (m b, b) -> m r
attemptOne2' f (ma , a) mbb@(_ , b) = (do a' <- ma
                                          return (f a' b)
                                      ) <<+ attemptOne1' (f a) mbb

attemptOne3' :: MonadCatch m => (a -> b -> c -> r) -> (m a, a) -> (m b, b) -> (m c, c) -> m r
attemptOne3' f (ma , a) mbb@(_ , b) mcc@(_ , c) = (do a' <- ma
                                                      return (f a' b c)
                                                  ) <<+ attemptOne2' (f a) mbb mcc

attemptOne4' :: MonadCatch m => (a -> b -> c -> d -> r) -> (m a, a) -> (m b, b) -> (m c, c) -> (m d, d) -> m r
attemptOne4' f (ma , a) mbb@(_ , b) mcc@(_ , c) mdd@(_ , d) = (do a' <- ma
                                                                  return (f a' b c d)
                                                              ) <<+ attemptOne3' (f a) mbb mcc mdd

attemptOne2 :: MonadCatch m => (a -> b -> r) -> m (m a, a) -> m (m b, b) -> m r
attemptOne2 f = liftArgument2 (attemptOne2' f)

attemptOne3 :: MonadCatch m => (a -> b -> c -> r) -> m (m a, a) -> m (m b, b) -> m (m c, c) -> m r
attemptOne3 f = liftArgument3 (attemptOne3' f)

attemptOne4 :: MonadCatch m => (a -> b -> c -> d -> r) -> m (m a, a) -> m (m b, b) -> m (m c, c) -> m (m d, d) -> m r
attemptOne4 f = liftArgument4 (attemptOne4' f)



newtype S s m a = S {runS :: s -> m (a, s)}
instance Monad m => Functor (S s m) where fmap = liftM
instance Monad m => Applicative (S s m) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = liftM2 ($)
instance Monad m => Monad (S s m) where
  {-# INLINE return #-}
  return a = S $ \b -> return (a, b)
  {-# INLINE (>>=) #-}
  m >>= k = S $ \_b -> runS m _b >>= \(a, _b) -> runS (k a) _b

attemptOneN :: (Traversable t, MonadCatch m) => (t a -> r) -> t (m (m a, a)) -> m r
attemptOneN f = liftM (f . fst) . flip runS False . Traversable.mapM each where
  each m = S $ \b -> m >>= \(ma, a) ->
    if b then return (a, b) else liftM (flip (,) True) ma <<+ return (a, b)

attemptOne1N :: (Traversable t, MonadCatch m) => (a -> t b -> r) -> m (m a, a) -> t (m (m b, b)) -> m r
attemptOne1N f mmaa mmbbs = do
  (ma, a) <- mmaa
  mbbs <- Traversable.sequence mmbbs
  (<<+) (flip liftM ma $ \a' -> f a' $ fmap snd mbbs) $
        attemptOneN (f a) (fmap return mbbs)

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

childLMofN :: (MonadCatch m, Node b, Traversable t) =>
  Int -> (t b -> a) -> t (c,b) -> ((c, Generic b) , Generic b -> m a)
childLMofN = \m f cbs ->
  childLaux (cbs `atIndex` m) $ \ b' -> f $ snd $
    Traversable.mapAccumL (\n (_, b) -> n `seq` (n + 1, if n == m then b' else b)) 0 cbs

  where
    -- helper function for looking up the nth element in a container
    atIndex :: Traversable t => t a -> Int -> a
    atIndex as n = snd $ Foldable.foldl' snoc (0, initial) as where
      initial = error "childLMofN: index too large!"
      snoc (m, found) a = m `seq` (,) (m + 1) $ if n == m then a else found

-------------------------------------------------------------------------------

liftArgument2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftArgument2 f ma mb = join (liftM2 f ma mb)

liftArgument3 :: Monad m => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
liftArgument3 f ma mb mc = join (liftM3 f ma mb mc)

liftArgument4 :: Monad m => (a -> b -> c -> d -> m e) -> m a -> m b -> m c -> m d -> m e
liftArgument4 f ma mb mc md = join (liftM4 f ma mb mc md)

liftArgumentN :: (Traversable t, Monad m) => (t a -> m b) -> t (m a) -> m b
liftArgumentN f mas = Traversable.sequence mas >>= f

liftArgument1N :: (Traversable t, Monad m) => (a -> t b -> m c) -> m a -> t (m b) -> m c
liftArgument1N f ma mbs = do a  <- ma
                             bs <- Traversable.sequence mbs
                             f a bs

-------------------------------------------------------------------------------
