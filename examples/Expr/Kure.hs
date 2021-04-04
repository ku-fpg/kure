{-# LANGUAGE CPP #-}
{-# LANGUAGE InstanceSigs, LambdaCase, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Expr.Kure where

import Control.Monad

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif

import Language.KURE

import Expr.AST
import Expr.Context

---------------------------------------------------------------------------

data Universe = GExpr Expr
              | GCmd Cmd

---------------------------------------------------------------------------

instance Injection Expr Universe where
  inject = GExpr

  project (GExpr e) = Just e
  project _         = Nothing

instance Injection Cmd Universe where
  inject = GCmd

  project (GCmd c) = Just c
  project _        = Nothing

---------------------------------------------------------------------------

instance (ExtendPath c Int, AddDef c) => Walker c Universe where
   allR :: MonadCatch m => Rewrite c m Universe -> Rewrite c m Universe
   allR r = prefixFailMsg "allR failed: " $
            rewrite $ \ c -> \case
              GExpr e  -> inject <$> applyR allRexpr c e
              GCmd cm  -> inject <$> applyR allRcmd c cm
     where
       allRexpr = readerT $ \case
                    Add _ _  -> addAllR (extractR r) (extractR r)
                    ESeq _ _ -> eseqAllR (extractR r) (extractR r)
                    _        -> idR

       allRcmd  = readerT $ \case
                    Seq _ _    -> seqAllR (extractR r) (extractR r)
                    Assign _ _ -> assignR (extractR r)

---------------------------------------------------------------------------

seqT :: (ExtendPath c Int, AddDef c, MonadFail m) => Transform c m Cmd a1 -> Transform c m Cmd a2 -> (a1 -> a2 -> b) -> Transform c m Cmd b
seqT t1 t2 f = transform $ \ c -> \case
                                     Seq cm1 cm2 -> f <$> applyT t1 (c @@ 0) cm1 <*> applyT t2 (updateContextCmd cm1 c @@ 1) cm2
                                     _           -> fail "not a Seq"

seqAllR :: (ExtendPath c Int, AddDef c, MonadFail m) => Rewrite c m Cmd -> Rewrite c m Cmd -> Rewrite c m Cmd
seqAllR r1 r2 = seqT r1 r2 Seq

seqAnyR :: (ExtendPath c Int, AddDef c, MonadCatch m) => Rewrite c m Cmd -> Rewrite c m Cmd -> Rewrite c m Cmd
seqAnyR r1 r2 = unwrapAnyR $ seqAllR (wrapAnyR r1) (wrapAnyR r2)

seqOneR :: (ExtendPath c Int, AddDef c, MonadCatch m) => Rewrite c m Cmd -> Rewrite c m Cmd -> Rewrite c m Cmd
seqOneR r1 r2 = unwrapOneR $ seqAllR (wrapOneR r1) (wrapOneR r2)

---------------------------------------------------------------------------

assignT :: (ExtendPath c Int, MonadFail m) => Transform c m Expr a -> (Name -> a -> b) -> Transform c m Cmd b
assignT t f = transform $ \ c -> \case
                                    Assign n e -> f n <$> applyT t (c @@ 0) e
                                    _          -> fail "not an Assign"

assignR :: (ExtendPath c Int, MonadFail m) => Rewrite c m Expr -> Rewrite c m Cmd
assignR r = assignT r Assign

---------------------------------------------------------------------------

varT :: MonadFail m => (Name -> b) -> Transform c m Expr b
varT f = contextfreeT $ \case
                           Var v -> return (f v)
                           _     -> fail "not a Var"

---------------------------------------------------------------------------

litT :: MonadFail m => (Int -> b) -> Transform c m Expr b
litT f = contextfreeT $ \case
                           Lit v -> return (f v)
                           _     -> fail "not a Lit"

---------------------------------------------------------------------------

addT :: (ExtendPath c Int, MonadFail m) => Transform c m Expr a1 -> Transform c m Expr a2 -> (a1 -> a2 -> b) -> Transform c m Expr b
addT t1 t2 f = transform $ \ c -> \case
                                     Add e1 e2 -> f <$> applyT t1 (c @@ 0) e1 <*> applyT t2 (c @@ 1) e2
                                     _         -> fail "not an Add"

addAllR :: (ExtendPath c Int, MonadFail m) => Rewrite c m Expr -> Rewrite c m Expr -> Rewrite c m Expr
addAllR r1 r2 = addT r1 r2 Add

addAnyR :: (ExtendPath c Int, MonadCatch m) => Rewrite c m Expr -> Rewrite c m Expr -> Rewrite c m Expr
addAnyR r1 r2 = unwrapAnyR $ addAllR (wrapAnyR r1) (wrapAnyR r2)

addOneR :: (ExtendPath c Int, MonadCatch m) => Rewrite c m Expr -> Rewrite c m Expr -> Rewrite c m Expr
addOneR r1 r2 = unwrapOneR $ addAllR (wrapOneR r1) (wrapOneR r2)

---------------------------------------------------------------------------

eseqT :: (ExtendPath c Int, AddDef c, MonadFail m) => Transform c m Cmd a1 -> Transform c m Expr a2 -> (a1 -> a2 -> b) -> Transform c m Expr b
eseqT t1 t2 f = transform $ \ c -> \case
                                      ESeq cm e1 -> f <$> applyT t1 (c @@ 0) cm <*> applyT t2 (updateContextCmd cm c @@ 1) e1
                                      _          -> fail "not an ESeq"

eseqAllR :: (ExtendPath c Int, AddDef c, MonadFail m) => Rewrite c m Cmd -> Rewrite c m Expr -> Rewrite c m Expr
eseqAllR r1 r2 = eseqT r1 r2 ESeq

eseqAnyR :: (ExtendPath c Int, AddDef c, MonadCatch m) => Rewrite c m Cmd -> Rewrite c m Expr -> Rewrite c m Expr
eseqAnyR r1 r2 = unwrapAnyR $ eseqAllR (wrapAnyR r1) (wrapAnyR r2)

eseqOneR :: (ExtendPath c Int, AddDef c, MonadCatch m) => Rewrite c m Cmd -> Rewrite c m Expr -> Rewrite c m Expr
eseqOneR r1 r2 = unwrapOneR $ eseqAllR (wrapOneR r1) (wrapOneR r2)

---------------------------------------------------------------------------
