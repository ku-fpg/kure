{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}

module ExprKure where

import Control.Applicative
import Data.Monoid

import Language.KURE
import Language.KURE.Injection
import Language.KURE.Utilities

import Expr

---------------------------------------------------------------------------

type TranslateE a b = Translate Context Maybe a b
type RewriteE a = TranslateE a a

---------------------------------------------------------------------------

data GenericExpr = GExpr Expr
                 | GCmd Cmd

instance Term GenericExpr where
  type Generic GenericExpr = GenericExpr

---------------------------------------------------------------------------

instance WalkerR Context Maybe GenericExpr where
  allR r = rewrite $ \ c g -> case g of
                                GExpr e -> allRgeneric r c e
                                GCmd cm -> allRgeneric r c cm

  anyR r = rewrite $ \ c g -> case g of
                                GExpr e -> anyRgeneric r c e
                                GCmd cm -> anyRgeneric r c cm

instance Monoid b => WalkerT Context Maybe GenericExpr b where
  crushT t = translate $ \ c g -> case g of
                                    GExpr e -> crushTgeneric t c e
                                    GCmd cm -> crushTgeneric t c cm

instance WalkerL Context Maybe GenericExpr where
  childL n = lens $ \ c g -> case g of
                               GExpr e -> childLgeneric n c e
                               GCmd cm -> childLgeneric n c cm

---------------------------------------------------------------------------

instance Injection Expr GenericExpr where
  inject = GExpr

  retract (GExpr e) = Just e
  retract _         = Nothing

instance Term Expr where
  type Generic Expr = GenericExpr

instance WalkerR Context Maybe Expr where
  allR r =  varT Var
         <+ litT Lit
         <+ addT (extractR r) (extractR r) Add
         <+ eseqT (extractR r) (extractR r) ESeq

  anyR r =  addT' (attemptExtractR r) (attemptExtractR r) (attemptAny2 Add)
         <+ eseqT' (attemptExtractR r) (attemptExtractR r) (attemptAny2 ESeq)
         <+ fail "anyR failed"

instance Monoid b => WalkerT Context Maybe Expr b where
  crushT t =  varT (const mempty)
           <+ litT (const mempty)
           <+ addT (extractT t) (extractT t) mappend

instance WalkerL Context Maybe Expr where
  childL 0 =  addT exposeT idR (childL0of2 Add)
           <+ eseqT exposeT idR (childL0of2 ESeq)
           <+ missingChildL 0

  childL 1 =  addT  idR exposeT (childL1of2 Add)
           <+ eseqT idR exposeT (childL1of2 ESeq)
           <+ missingChildL 1

  childL n = missingChildL n

---------------------------------------------------------------------------

instance Injection Cmd GenericExpr where
  inject = GCmd

  retract (GCmd c) = Just c
  retract _        = Nothing

instance Term Cmd where
  type Generic Cmd = GenericExpr

instance WalkerR Context Maybe Cmd where
  allR r =  seqT (extractR r) (extractR r) Seq
         <+ assignT (extractR r) Assign

  anyR r =  seqT' (attemptExtractR r) (attemptExtractR r) (attemptAny2 Seq)
         <+ assignT (extractR r) Assign
         <+ fail "anyR failed"

instance Monoid b => WalkerT Context Maybe Cmd b where
  crushT t =  seqT (extractT t) (extractT t) mappend
           <+ assignT (extractT t) (\ _ -> id)

instance WalkerL Context Maybe Cmd where
  childL 0 =  seqT exposeT idR (childL0of2 Seq)
           <+ assignT exposeT (childL1of2 Assign)

  childL 1 =  seqT idR exposeT (childL1of2 Seq)
           <+ missingChildL 1

  childL n = missingChildL n

---------------------------------------------------------------------------

seqT' :: TranslateE Cmd a1 -> TranslateE Cmd a2 -> (Maybe a1 -> Maybe a2 -> Maybe b) -> TranslateE Cmd b
seqT' t1 t2 f = translate $ \ c cm -> case cm of
                                       Seq cm1 cm2 -> f (apply t1 c cm1) (apply t2 (updateContext cm1 c) cm2)
                                       _           -> fail "not a Seq"

seqT :: TranslateE Cmd a1 -> TranslateE Cmd a2 -> (a1 -> a2 -> b) -> TranslateE Cmd b
seqT t1 t2 f = seqT' t1 t2 (liftA2 f)

assignT :: TranslateE Expr a -> (Name -> a -> b) -> TranslateE Cmd b
assignT t f = translate $ \ c cm -> case cm of
                                      Assign n e -> f n <$> apply t c e
                                      _          -> fail "not an Assign"

varT :: (Name -> b) -> TranslateE Expr b
varT f = liftMT $ \ e -> case e of
                           Var v -> pure (f v)
                           _     -> fail "not a Var"

litT :: (Int -> b) -> TranslateE Expr b
litT f = liftMT $ \ e -> case e of
                           Lit v -> pure (f v)
                           _     -> fail "not a Lit"

addT' :: TranslateE Expr a1 -> TranslateE Expr a2 -> (Maybe a1 -> Maybe a2 -> Maybe b) -> TranslateE Expr b
addT' t1 t2 f = translate $ \ c e -> case e of
                                       Add e1 e2 -> f (apply t1 c e1) (apply t2 c e2)
                                       _         -> fail "not an Add"

addT :: TranslateE Expr a1 -> TranslateE Expr a2 -> (a1 -> a2 -> b) -> TranslateE Expr b
addT t1 t2 f = addT' t1 t2 (liftA2 f)

eseqT' :: TranslateE Cmd a1 -> TranslateE Expr a2 -> (Maybe a1 -> Maybe a2 -> Maybe b) -> TranslateE Expr b
eseqT' t1 t2 f = translate $ \ c e -> case e of
                                        ESeq cm e1 -> f (apply t1 c cm) (apply t2 (updateContext cm c) e1)
                                        _          -> fail "not an ESeq"

eseqT :: TranslateE Cmd a1 -> TranslateE Expr a2 -> (a1 -> a2 -> b) -> TranslateE Expr b
eseqT t1 t2 f = eseqT' t1 t2 (liftA2 f)

---------------------------------------------------------------------------
