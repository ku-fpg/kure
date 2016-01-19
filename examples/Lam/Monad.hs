{-# LANGUAGE CPP #-}
{-# LANGUAGE InstanceSigs #-}

module Lam.Monad where

import Language.KURE

import Control.Exception (PatternMatchFail(..))
import Control.Monad
import Control.Monad.Catch
#if __GLASGOW_HASKELL__ >= 800
import Control.Monad.Fail (MonadFail(..))
#endif

-------------------------------------------------------------------------------

newtype LamM a = LamM {lamM :: Int -> (Int, Either SomeException a)}

runLamM :: LamM a -> Either SomeException a
runLamM m = snd (lamM m 0)

instance Functor LamM where
  fmap :: (a -> b) -> LamM a -> LamM b
  fmap = liftM

instance Applicative LamM where
  pure :: a -> LamM a
  pure a = LamM (\n -> (n,Right a))

  (<*>) :: LamM (a -> b) -> LamM a -> LamM b
  (<*>) = ap

instance Monad LamM where
  fail :: String -> LamM a
  fail msg = LamM (\ n -> (n, Left (SomeException $ PatternMatchFail msg)))

  (>>=) :: LamM a -> (a -> LamM b) -> LamM b
  (LamM f) >>= gg = LamM $ \ n -> case f n of
                                    (n', Left e)  -> (n', Left e)
                                    (n', Right a) -> lamM (gg a) n'

#if __GLASGOW_HASKELL__ >= 800
instance MonadFail LamM where
  fail :: String -> LamM a
  fail msg = LamM (\ n -> (n, Left (SomeException $ PatternMatchFail msg)))
#endif

instance MonadThrow LamM where
  throwM :: Exception e => e -> LamM a
  throwM e = LamM $ \i -> (i, throwM e)

instance MonadCatch LamM where
  catch :: Exception e => LamM a -> (e -> LamM a) -> LamM a
  l@(LamM st) `catch` f = LamM $ \ n ->
    case st n of
      (n', Left e)  -> lamM (case fromException e of
                                  Just ex -> f ex
                                  Nothing -> l
                            ) n'
      (n', Right a) -> (n', Right a)

-------------------------------------------------------------------------------
