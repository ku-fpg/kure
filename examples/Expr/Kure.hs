{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Expr.Kure where

import Control.Monad

import Language.KURE

import Expr.AST
import Expr.Context

---------------------------------------------------------------------------

data Generic = GExpr Expr
             | GCmd Cmd

---------------------------------------------------------------------------

instance Injection Expr Generic where
  inject = GExpr

  project (GExpr e) = Just e
  project _         = Nothing

instance Injection Cmd Generic where
  inject = GCmd

  project (GCmd c) = Just c
  project _        = Nothing

---------------------------------------------------------------------------

instance (ExtendPath c Int, AddDef c) => Walker c Generic where
-- allR :: MonadCatch m => Rewrite Context m Generic -> Rewrite Context m Generic
   allR r = prefixFailMsg "allR failed: " $
            rewrite $ \ c g -> case g of
              GExpr e  -> inject <$> apply allRexpr c e
              GCmd cm  -> inject <$> apply allRcmd c cm
     where
       allRexpr = readerT $ \ expr -> case expr of
                    Add _ _  -> addAllR (extractR r) (extractR r)
                    ESeq _ _ -> eseqAllR (extractR r) (extractR r)
                    _        -> idR

       allRcmd  = readerT $ \ cmd -> case cmd of
                    Seq _ _    -> seqAllR (extractR r) (extractR r)
                    Assign _ _ -> assignR (extractR r)

---------------------------------------------------------------------------

seqT :: (ExtendPath c Int, AddDef c, Monad m) => Translate c m Cmd a1 -> Translate c m Cmd a2 -> (a1 -> a2 -> b) -> Translate c m Cmd b
seqT t1 t2 f = translate $ \ c cm -> case cm of
                                       Seq cm1 cm2 -> f <$> apply t1 (c @@ 0) cm1 <*> apply t2 (updateContextCmd cm1 c @@ 1) cm2
                                       _           -> fail "not a Seq"

seqAllR :: (ExtendPath c Int, AddDef c, Monad m) => Rewrite c m Cmd -> Rewrite c m Cmd -> Rewrite c m Cmd
seqAllR r1 r2 = seqT r1 r2 Seq

seqAnyR :: (ExtendPath c Int, AddDef c, MonadCatch m) => Rewrite c m Cmd -> Rewrite c m Cmd -> Rewrite c m Cmd
seqAnyR r1 r2 = unwrapAnyR $ seqAllR (wrapAnyR r1) (wrapAnyR r2)

seqOneR :: (ExtendPath c Int, AddDef c, MonadCatch m) => Rewrite c m Cmd -> Rewrite c m Cmd -> Rewrite c m Cmd
seqOneR r1 r2 = unwrapOneR $ seqAllR (wrapOneR r1) (wrapOneR r2)

---------------------------------------------------------------------------

assignT :: (ExtendPath c Int, Monad m) => Translate c m Expr a -> (Name -> a -> b) -> Translate c m Cmd b
assignT t f = translate $ \ c cm -> case cm of
                                      Assign n e -> f n <$> apply t (c @@ 0) e
                                      _          -> fail "not an Assign"

assignR :: (ExtendPath c Int, Monad m) => Rewrite c m Expr -> Rewrite c m Cmd
assignR r = assignT r Assign

---------------------------------------------------------------------------

varT :: Monad m => (Name -> b) -> Translate c m Expr b
varT f = contextfreeT $ \ e -> case e of
                                 Var v -> return (f v)
                                 _     -> fail "not a Var"

---------------------------------------------------------------------------

litT :: Monad m => (Int -> b) -> Translate c m Expr b
litT f = contextfreeT $ \ e -> case e of
                                 Lit v -> return (f v)
                                 _     -> fail "not a Lit"

---------------------------------------------------------------------------

addT :: (ExtendPath c Int, Monad m) => Translate c m Expr a1 -> Translate c m Expr a2 -> (a1 -> a2 -> b) -> Translate c m Expr b
addT t1 t2 f = translate $ \ c e -> case e of
                                      Add e1 e2 -> f <$> apply t1 (c @@ 0) e1 <*> apply t2 (c @@ 1) e2
                                      _         -> fail "not an Add"

addAllR :: (ExtendPath c Int, Monad m) => Rewrite c m Expr -> Rewrite c m Expr -> Rewrite c m Expr
addAllR r1 r2 = addT r1 r2 Add

addAnyR :: (ExtendPath c Int, MonadCatch m) => Rewrite c m Expr -> Rewrite c m Expr -> Rewrite c m Expr
addAnyR r1 r2 = unwrapAnyR $ addAllR (wrapAnyR r1) (wrapAnyR r2)

addOneR :: (ExtendPath c Int, MonadCatch m) => Rewrite c m Expr -> Rewrite c m Expr -> Rewrite c m Expr
addOneR r1 r2 = unwrapOneR $ addAllR (wrapOneR r1) (wrapOneR r2)

---------------------------------------------------------------------------

eseqT :: (ExtendPath c Int, AddDef c, Monad m) => Translate c m Cmd a1 -> Translate c m Expr a2 -> (a1 -> a2 -> b) -> Translate c m Expr b
eseqT t1 t2 f = translate $ \ c e -> case e of
                                       ESeq cm e1 -> f <$> apply t1 (c @@ 0) cm <*> apply t2 (updateContextCmd cm c @@ 1) e1
                                       _          -> fail "not an ESeq"

eseqAllR :: (ExtendPath c Int, AddDef c, Monad m) => Rewrite c m Cmd -> Rewrite c m Expr -> Rewrite c m Expr
eseqAllR r1 r2 = eseqT r1 r2 ESeq

eseqAnyR :: (ExtendPath c Int, AddDef c, MonadCatch m) => Rewrite c m Cmd -> Rewrite c m Expr -> Rewrite c m Expr
eseqAnyR r1 r2 = unwrapAnyR $ eseqAllR (wrapAnyR r1) (wrapAnyR r2)

eseqOneR :: (ExtendPath c Int, AddDef c, MonadCatch m) => Rewrite c m Cmd -> Rewrite c m Expr -> Rewrite c m Expr
eseqOneR r1 r2 = unwrapOneR $ eseqAllR (wrapOneR r1) (wrapOneR r2)

---------------------------------------------------------------------------

-- I find it annoying that Applicative is not a superclass of Monad.
(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM
{-# INLINE (<$>) #-}

(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap
{-# INLINE (<*>) #-}

---------------------------------------------------------------------------
