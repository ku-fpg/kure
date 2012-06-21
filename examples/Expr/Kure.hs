{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}

module Expr.Kure where

import Control.Applicative

import Data.Monoid

import Language.KURE
import Language.KURE.Injection
import Language.KURE.Utilities

import Expr.AST

---------------------------------------------------------------------------

-- NOTE: allT, allR and anyR have been defined to serve as examples,
--       but using the default instances would be fine (just slightly less efficient).

---------------------------------------------------------------------------

type TranslateE a b = Translate Context Maybe a b
type RewriteE a = TranslateE a a

applyE :: TranslateE a b -> a -> Maybe b
applyE t = apply t initialContext

---------------------------------------------------------------------------

data GenericExpr = GExpr Expr
                 | GCmd Cmd

instance Term GenericExpr where
  type Generic GenericExpr = GenericExpr

  numChildren (GExpr e) = numChildren e
  numChildren (GCmd c)  = numChildren c

---------------------------------------------------------------------------

instance Walker Context Maybe GenericExpr where

  childL n = lens $ translate $ \ c g -> case g of
                                           GExpr e -> childLgeneric n c e
                                           GCmd cm -> childLgeneric n c cm

  allT t = translate $ \ c g -> case g of
                                  GExpr e -> allTgeneric t c e
                                  GCmd cm -> allTgeneric t c cm

  allR r = rewrite $ \ c g -> case g of
                                GExpr e -> allRgeneric r c e
                                GCmd cm -> allRgeneric r c cm

  anyR r = rewrite $ \ c g -> case g of
                                GExpr e -> anyRgeneric r c e
                                GCmd cm -> anyRgeneric r c cm

---------------------------------------------------------------------------

instance Injection Expr GenericExpr where
  inject = GExpr

  retract (GExpr e) = Just e
  retract _         = Nothing

instance Term Expr where
  type Generic Expr = GenericExpr

  numChildren (Add _ _)  = 2
  numChildren (ESeq _ _) = 2
  numChildren (Var _)    = 0
  numChildren (Lit _)    = 0


instance Walker Context Maybe Expr where
  childL n = lens $
    case n of
      0 ->    addT  exposeT idR (childL0of2 Add)
           <+ eseqT exposeT idR (childL0of2 ESeq)
      1 ->    addT  idR exposeT (childL1of2 Add)
           <+ eseqT idR exposeT (childL1of2 ESeq)
      _ -> fail (missingChild n)

  allT t =  varT (\ _ -> mempty)
         <+ litT (\ _ -> mempty)
         <+ addT (extractT t) (extractT t) mappend
         <+ eseqT (extractT t) (extractT t) mappend

  allR r =  varT Var
         <+ litT Lit
         <+ addAllR (extractR r) (extractR r)
         <+ eseqAllR (extractR r) (extractR r)

  anyR r =  addAnyR (extractR r) (extractR r)
         <+ eseqAnyR (extractR r) (extractR r)
         <+ fail "anyR failed"

---------------------------------------------------------------------------

instance Injection Cmd GenericExpr where
  inject = GCmd

  retract (GCmd c) = Just c
  retract _        = Nothing

instance Term Cmd where
  type Generic Cmd = GenericExpr

  numChildren (Seq _ _)    = 2
  numChildren (Assign _ _) = 2

instance Walker Context Maybe Cmd where
  childL n = lens $
    case n of
      0 ->    seqT exposeT idR (childL0of2 Seq)
           <+ assignT exposeT (childL1of2 Assign)
      1 -> seqT idR exposeT (childL1of2 Seq)
      _ -> fail (missingChild n)

  allT t =  seqT (extractT t) (extractT t) mappend
         <+ assignT (extractT t) (\ _ -> id)

  allR r =  seqAllR (extractR r) (extractR r)
         <+ assignR (extractR r)

  anyR r =  seqAnyR (extractR r) (extractR r)
         <+ assignR (extractR r)
         <+ fail "anyR failed"

---------------------------------------------------------------------------

seqT' :: TranslateE Cmd a1 -> TranslateE Cmd a2 -> (Maybe a1 -> Maybe a2 -> Maybe b) -> TranslateE Cmd b
seqT' t1 t2 f = translate $ \ c cm -> case cm of
                                       Seq cm1 cm2 -> f (apply t1 (c @@ 0) cm1) (apply t2 (updateContextCmd cm1 c @@ 1) cm2)
                                       _           -> fail "not a Seq"

seqT :: TranslateE Cmd a1 -> TranslateE Cmd a2 -> (a1 -> a2 -> b) -> TranslateE Cmd b
seqT t1 t2 f = seqT' t1 t2 (liftA2 f)

seqAllR :: RewriteE Cmd -> RewriteE Cmd -> RewriteE Cmd
seqAllR r1 r2 = seqT r1 r2 Seq

seqAnyR :: RewriteE Cmd -> RewriteE Cmd -> RewriteE Cmd
seqAnyR r1 r2 = seqT' (attemptR r1) (attemptR r2) (attemptAny2 Seq)

---------------------------------------------------------------------------

assignT :: TranslateE Expr a -> (Name -> a -> b) -> TranslateE Cmd b
assignT t f = translate $ \ c cm -> case cm of
                                      Assign n e -> f n <$> apply t (c @@ 0) e
                                      _          -> fail "not an Assign"

assignR :: RewriteE Expr -> RewriteE Cmd
assignR r = assignT r Assign

---------------------------------------------------------------------------

varT :: (Name -> b) -> TranslateE Expr b
varT f = contextfreeT $ \ e -> case e of
                                 Var v -> pure (f v)
                                 _     -> fail "not a Var"

---------------------------------------------------------------------------

litT :: (Int -> b) -> TranslateE Expr b
litT f = contextfreeT $ \ e -> case e of
                                 Lit v -> pure (f v)
                                 _     -> fail "not a Lit"

---------------------------------------------------------------------------

addT' :: TranslateE Expr a1 -> TranslateE Expr a2 -> (Maybe a1 -> Maybe a2 -> Maybe b) -> TranslateE Expr b
addT' t1 t2 f = translate $ \ c e -> case e of
                                       Add e1 e2 -> f (apply t1 (c @@ 0) e1) (apply t2 (c @@ 1) e2)
                                       _         -> fail "not an Add"

addT :: TranslateE Expr a1 -> TranslateE Expr a2 -> (a1 -> a2 -> b) -> TranslateE Expr b
addT t1 t2 f = addT' t1 t2 (liftA2 f)

addAllR :: RewriteE Expr -> RewriteE Expr -> RewriteE Expr
addAllR r1 r2 = addT r1 r2 Add

addAnyR :: RewriteE Expr -> RewriteE Expr -> RewriteE Expr
addAnyR r1 r2 = addT' (attemptR r1) (attemptR r2) (attemptAny2 Add)

---------------------------------------------------------------------------

eseqT' :: TranslateE Cmd a1 -> TranslateE Expr a2 -> (Maybe a1 -> Maybe a2 -> Maybe b) -> TranslateE Expr b
eseqT' t1 t2 f = translate $ \ c e -> case e of
                                        ESeq cm e1 -> f (apply t1 (c @@ 0) cm) (apply t2 (updateContextCmd cm c @@ 1) e1)
                                        _          -> fail "not an ESeq"

eseqT :: TranslateE Cmd a1 -> TranslateE Expr a2 -> (a1 -> a2 -> b) -> TranslateE Expr b
eseqT t1 t2 f = eseqT' t1 t2 (liftA2 f)

eseqAllR :: RewriteE Cmd -> RewriteE Expr -> RewriteE Expr
eseqAllR r1 r2 = eseqT r1 r2 ESeq

eseqAnyR :: RewriteE Cmd -> RewriteE Expr -> RewriteE Expr
eseqAnyR r1 r2 = eseqT' (attemptR r1) (attemptR r2) (attemptAny2 ESeq)

---------------------------------------------------------------------------
