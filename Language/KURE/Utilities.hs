-- |
-- Module: Language.KURE.Utilities
-- Copyright: (c) 2006-2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: alpha
-- Portability: ghc
--
-- This module contains several utility functions that can be useful to users of KURE,
-- when definining instances of the KURE classes.

module Language.KURE.Utilities where

import Control.Applicative

import Language.KURE.Translate
import Language.KURE.Walker
import Language.KURE.Injection

-------------------------------------------------------------------------------

-- | These functions are to aid with defining 'WalkerR', 'WalkerT' and 'WalkerL' instances
--   for the 'Generic' type.  See the "expr" example.

allRgeneric :: WalkerR c m a => Rewrite c m (Generic a) -> c -> a -> m (Generic a)
allRgeneric r c a = inject <$> apply (allR r) c a

anyRgeneric :: WalkerR c m a => Rewrite c m (Generic a) -> c -> a -> m (Generic a)
anyRgeneric r c a = inject <$> apply (anyR r) c a

crushTgeneric :: WalkerT c m a b => Translate c m (Generic a) b -> c -> a -> m b
crushTgeneric t = apply (crushT t)

chooseLgeneric :: WalkerL c m a => Int -> c -> a -> m ((c, Generic a), Generic a -> m (Generic a))
chooseLgeneric n c a = (second.result.liftA) inject <$> apply (chooseL n) c a

-------------------------------------------------------------------------------

-- | These are useful in conjunction with scoping combinators to define anyR instances.
--   See the Exp example or the HERMIT package.

attemptAny2 :: Monad m => (a1 -> a2 -> r) -> m (Bool,a1) -> m (Bool,a2) -> m r
attemptAny2 f mba1 mba2 = do (b1,a1) <- mba1
                             (b2,a2) <- mba2
                             if b1 || b2
                              then return (f a1 a2)
                              else fail "anyR failed for both children."

attemptAny3 :: Monad m => (a1 -> a2 -> a3 -> r) -> m (Bool,a1) -> m (Bool,a2) -> m (Bool,a3) -> m r
attemptAny3 f mba1 mba2 mba3 = do (b1,a1) <- mba1
                                  (b2,a2) <- mba2
                                  (b3,a3) <- mba3
                                  if or [b1,b2,b3]
                                   then return (f a1 a2 a3)
                                   else fail "anyR failed for all three children."

attemptAnyN :: (Functor m, Monad m) => ([a] -> b) -> [m (Bool,a)] -> m b
attemptAnyN f mbas = do (bs,as) <- unzip <$> sequence mbas
                        if or bs
                         then return (f as)
                         else fail ("anyR failed for all " ++ show (length bs) ++ " children.")

attemptAny1N :: (Functor m, Monad m) => (a1 -> [a2] -> r) -> m (Bool,a1) -> [m (Bool,a2)] -> m r
attemptAny1N f mba mbas = do (b ,a)  <- mba
                             (bs,as) <- unzip <$> sequence mbas
                             if or (b:bs)
                               then return (f a as)
                               else fail ("anyR failed for all " ++ show (1 + length bs) ++ " children.")

-------------------------------------------------------------------------------

missingChildL :: Monad m => Int -> Lens c m a b
missingChildL n = fail ("There is no child number " ++ show n ++ ".")

-------------------------------------------------------------------------------

-- | These functions are helpful when defining WalkerL instances in combination with scoping combinators.
--   See the Expr example.
--   Unfortunately they increase quadratically with the number of fields of the constructor.
--   It would be nice if they were further expanded to include the calls of idR and exposeT;
--   however this would create a plethora of additional cases as the number (and positions)
--   of interesting children would be additional dimensions.
--   Note that the numbering scheme MofN is that N is the number of children (including uninteresting children)
--   and M is the index of the chosen child, starting with index 0.  Thus M ranges from 0 to (n-1).

chooseLaux :: (Alternative m, Term b) => (c,b) -> (b -> a) -> ((c, Generic b), Generic b -> m a)
chooseLaux cb g = (second inject cb, retractWithA g)

chooseL0of1 :: (Alternative m, Term b) => (b -> a) -> (c,b) -> ((c, Generic b) , Generic b -> m a)
chooseL0of1 f cb = chooseLaux cb f

chooseL0of2 :: (Alternative m, Term b0) => (b0 -> b1 -> a) -> (c,b0) -> b1 -> ((c, Generic b0) , Generic b0 -> m a)
chooseL0of2 f cb0 b1 = chooseLaux cb0 (\ b0 -> f b0 b1)

chooseL1of2 :: (Alternative m, Term b1) => (b0 -> b1 -> a) -> b0 -> (c,b1) -> ((c, Generic b1) , Generic b1 -> m a)
chooseL1of2 f b0 cb1 = chooseLaux cb1 (\ b1 -> f b0 b1)

chooseL0of3 :: (Alternative m, Term b0) => (b0 -> b1 -> b2 -> a) -> (c,b0) -> b1 -> b2 -> ((c, Generic b0) , Generic b0 -> m a)
chooseL0of3 f cb0 b1 b2 = chooseLaux cb0 (\ b0 -> f b0 b1 b2)

chooseL1of3 :: (Alternative m, Term b1) => (b0 -> b1 -> b2 -> a) -> b0 -> (c,b1) -> b2 -> ((c, Generic b1) , Generic b1 -> m a)
chooseL1of3 f b0 cb1 b2 = chooseLaux cb1 (\ b1 -> f b0 b1 b2)

chooseL2of3 :: (Alternative m, Term b2) => (b0 -> b1 -> b2 -> a) -> b0 -> b1 -> (c,b2) -> ((c, Generic b2) , Generic b2 -> m a)
chooseL2of3 f b0 b1 cb2 = chooseLaux cb2 (\ b2 -> f b0 b1 b2)

chooseL0of4 :: (Alternative m, Term b0) => (b0 -> b1 -> b2 -> b3 -> a) -> (c,b0) -> b1 -> b2 -> b3 -> ((c, Generic b0) , Generic b0 -> m a)
chooseL0of4 f cb0 b1 b2 b3 = chooseLaux cb0 (\ b0 -> f b0 b1 b2 b3)

chooseL1of4 :: (Alternative m, Term b1) => (b0 -> b1 -> b2 -> b3 -> a) -> b0 -> (c,b1) -> b2 -> b3 -> ((c, Generic b1) , Generic b1 -> m a)
chooseL1of4 f b0 cb1 b2 b3 = chooseLaux cb1 (\ b1 -> f b0 b1 b2 b3)

chooseL2of4 :: (Alternative m, Term b2) => (b0 -> b1 -> b2 -> b3 -> a) -> b0 -> b1 -> (c,b2) -> b3 -> ((c, Generic b2) , Generic b2 -> m a)
chooseL2of4 f b0 b1 cb2 b3 = chooseLaux cb2 (\ b2 -> f b0 b1 b2 b3)

chooseL3of4 :: (Alternative m, Term b3) => (b0 -> b1 -> b2 -> b3 -> a) -> b0 -> b1 -> b2 -> (c,b3) -> ((c, Generic b3) , Generic b3 -> m a)
chooseL3of4 f b0 b1 b2 cb3 = chooseLaux cb3 (\ b3 -> f b0 b1 b2 b3)

chooseLMofN :: (Alternative m, Term b) => Int -> ([b] -> a) -> [(c,b)] -> ((c, Generic b) , Generic b -> m a)
chooseLMofN m f cbs = chooseLaux (cbs !! m) (\ b' -> f $ atIndex m (const b') (map snd cbs))

-------------------------------------------------------------------------------

-- | modify the value in a list at specified index.
atIndex :: Int -> (a -> a) -> [a] -> [a]
atIndex i f as = [ if n == i then f a else a
                 | (a,n) <- zip as [0..]
                 ]

-------------------------------------------------------------------------------

-- | Some of Conal Elliott's semantic editor combinators.

result :: (b -> c) -> (a -> b) -> (a -> c)
result = (.)

second :: (a -> b) -> (c,a) -> (c,b)
second f (c,a) = (c, f a)

-------------------------------------------------------------------------------
