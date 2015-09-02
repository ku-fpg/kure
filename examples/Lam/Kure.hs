{-# LANGUAGE CPP, InstanceSigs, LambdaCase, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lam.Kure where

import Control.Monad

import Language.KURE

import Lam.AST
import Lam.Context

-------------------------------------------------------------------------------

instance (ExtendPath c Crumb, AddBoundVar c) => Walker c Exp where
   allR :: MonadCatch m => Rewrite c m Exp -> Rewrite c m Exp
   allR r = modExc (stackStrategyFailure "allR") $
            appAllR r r <+> lamR r <+> idR

-------------------------------------------------------------------------------

-- | Congruence combinators.
--   Using these ensures that the context is updated consistantly.

varT :: MonadThrow m => (Name -> b) -> Transform c m Exp b
varT f = contextfreeT $ \case
                           Var n -> return (f n)
                           _     -> throwM (nodeMismatch "Var")

-------------------------------------------------------------------------------

lamT :: (ExtendPath c Crumb, AddBoundVar c, MonadThrow m) => Transform c m Exp a -> (Name -> a -> b) -> Transform c m Exp b
lamT t f = transform $ \ c -> \case
                                 Lam v e -> f v <$> applyT t (addBoundVar v c @@ Lam_Body) e
                                 _       -> throwM (nodeMismatch "Lam")

lamR :: (ExtendPath c Crumb, AddBoundVar c, MonadThrow m) => Rewrite c m Exp -> Rewrite c m Exp
lamR r = lamT r Lam

-------------------------------------------------------------------------------

appT :: (ExtendPath c Crumb, MonadThrow m) => Transform c m Exp a1 -> Transform c m Exp a2 -> (a1 -> a2 -> b) -> Transform c m Exp b
appT t1 t2 f = transform $ \ c -> \case
                                     App e1 e2 -> f <$> applyT t1 (c @@ App_Fun) e1 <*> applyT t2 (c @@ App_Arg) e2
                                     _         -> throwM (nodeMismatch "App")

appAllR :: (ExtendPath c Crumb, MonadThrow m) => Rewrite c m Exp -> Rewrite c m Exp -> Rewrite c m Exp
appAllR r1 r2 = appT r1 r2 App

appAnyR :: (ExtendPath c Crumb, MonadCatch m) => Rewrite c m Exp -> Rewrite c m Exp -> Rewrite c m Exp
appAnyR r1 r2 = unwrapAnyR $ appAllR (wrapAnyR r1) (wrapAnyR r2)

appOneR :: (ExtendPath c Crumb, MonadCatch m) => Rewrite c m Exp -> Rewrite c m Exp -> Rewrite c m Exp
appOneR r1 r2 = unwrapOneR $ appAllR (wrapOneR r1) (wrapOneR r2)

-------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ <= 708
(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM
{-# INLINE (<$>) #-}

(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap
{-# INLINE (<*>) #-}
#endif

-------------------------------------------------------------------------------
