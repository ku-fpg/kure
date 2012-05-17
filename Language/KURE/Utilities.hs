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

-------------------------------------------------------------------------------

missingChildL :: Monad m => Int -> Lens c m a b
missingChildL n = fail ("There is no child number " ++ show n ++ ".")

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


attemptAnyN :: (Applicative m, Monad m) => ([a] -> b) -> [m (Bool,a)] -> m b
attemptAnyN f mbas = do (bs,as) <- unzip <$> sequence mbas
                        if or bs
                         then return (f as)
                         else fail ("anyR failed for all " ++ show (length bs) ++ " children.")

attemptAny1N :: (Applicative m, Monad m) => (a1 -> [a2] -> r) -> m (Bool,a1) -> [m (Bool,a2)] -> m r
attemptAny1N f mba mbas = do (b ,a)  <- mba
                             (bs,as) <- unzip <$> sequence mbas
                             if or (b:bs)
                               then return (f a as)
                               else fail ("anyR failed for all " ++ show (1 + length bs) ++ " children.")

-------------------------------------------------------------------------------

-- One of Conal Elliott's semantic editor combinators.
result :: (b -> c) -> (a -> b) -> (a -> c)
result = (.)

-------------------------------------------------------------------------------
