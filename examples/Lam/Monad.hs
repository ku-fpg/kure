{-# LANGUAGE CPP #-}
{-# Language InstanceSigs #-}

module Lam.Monad where

import Language.KURE

import Control.Applicative
import Control.Monad

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail
#endif

-------------------------------------------------------------------------------

newtype LamM a = LamM {lamM :: Int -> (Int, Either String a)}

runLamM :: LamM a -> Either String a
runLamM m = snd (lamM m 0)

instance Monad LamM where
  return :: a -> LamM a
  return a = LamM (\n -> (n,Right a))

  (>>=) :: LamM a -> (a -> LamM b) -> LamM b
  (LamM f) >>= gg = LamM $ \ n -> case f n of
                                    (n', Left msg) -> (n', Left msg)
                                    (n', Right a)  -> lamM (gg a) n'

#if !MIN_VERSION_base(4,13,0)
  fail :: String -> LamM a
  fail msg = LamM (\ n -> (n, Left msg))
#endif

instance MonadFail LamM where
  fail :: String -> LamM a
  fail msg = LamM (\ n -> (n, Left msg))

instance MonadCatch LamM where
  catchM :: LamM a -> (String -> LamM a) -> LamM a
  (LamM st) `catchM` f = LamM $ \ n -> case st n of
                                        (n', Left msg) -> lamM (f msg) n'
                                        (n', Right a)  -> (n', Right a)

instance Functor LamM where
  fmap :: (a -> b) -> LamM a -> LamM b
  fmap = liftM

instance Applicative LamM where
  pure :: a -> LamM a
  pure  = return

  (<*>) :: LamM (a -> b) -> LamM a -> LamM b
  (<*>) = ap

-------------------------------------------------------------------------------
