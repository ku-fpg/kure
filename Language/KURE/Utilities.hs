-- |
-- Module: Language.KURE.Utilities
-- Copyright: (c) 2006-2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: alpha
-- Portability: ghc
--
-- This module contains several utility functions that are either used internally within KURE,
-- or that can be useful to users of KURE.

module Language.KURE.Utilities where

import Control.Applicative

import Language.KURE.Translate
import Language.KURE.Term
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

-- Some of Conal Elliott's semantic editor combinators.

result :: (b -> c) -> (a -> b) -> (a -> c)
result = (.)

second :: (a -> b) -> (c,a) -> (c,b)
second f (c,a) = (c, f a)

-------------------------------------------------------------------------------
