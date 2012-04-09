{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module ExprKURE where

import Control.Monad
import Data.Copointed
import Data.Monoid

import Language.KURE
import ExprLanguage

instance Copointed Ctxt where
  copoint (Ctxt (a,c)) = a 

instance EndoFunctor Ctxt where
  liftC f (Ctxt (a,c)) = Ctxt (f a, c)

instance InjectiveFunctor Ctxt where
  injectC  (Ctxt (a,c)) = Ctxt (inject a,c)
  retractC (Ctxt (a,c)) = fmap (\ a' -> Ctxt (a',c)) (retract a)


data GenericExpr = GExpr Expr
                 | GCmd Cmd


instance Injection GenericExpr GenericExpr where
  inject = id
  retract = Just

instance Term GenericExpr where
  type Generic GenericExpr = GenericExpr
  

instance Injection Expr GenericExpr where
  inject = GExpr
  retract (GExpr e) = Just e
  retract _         = Nothing

instance Term Expr where
  type Generic Expr = GenericExpr
  
instance Walker Ctxt Maybe Expr where
  allR gr = rewrite $ \ (Ctxt (e,c)) -> case e of
                                         (Lit n) -> return (Lit n)
                                         (Var v) -> return (Var v)
                                         (Add e1 e2) -> liftM2 Add  (apply (extractR gr) (Ctxt (e1,c))) (apply (extractR gr) (Ctxt (e2,c)))
                                         (ESeq cm e) -> liftM2 ESeq (apply (extractR gr) (Ctxt (cm,c))) (apply (extractR gr) (Ctxt (e,c)))
                                         
  crushT gt = translate $ \ (Ctxt (e,c)) -> case e of                     
                                              (Lit n) -> return mempty
                                              (Var v) -> return mempty
                                              (Add e1 e2) -> liftM2 mappend (apply (extractT gt) (Ctxt (e1,c))) (apply (extractT gt) (Ctxt (e2,c)))
                                              (ESeq cm e) -> liftM2 mappend (apply (extractT gt) (Ctxt (cm,c))) (apply (extractT gt) (Ctxt (e,c)))
  
  
instance Injection Cmd GenericExpr where
  inject = GCmd
  retract (GCmd c) = Just c
  retract _        = Nothing

instance Term Cmd where
  type Generic Cmd = GenericExpr
  
instance Walker Ctxt Maybe Cmd where
  allR gr = rewrite $ \ (Ctxt (cm,c)) -> case cm of
                                           (Assign v e)  -> liftM (Assign v) (apply (extractR gr) (Ctxt (e,c)))
                                           (Seq cm1 cm2) -> liftM2 Seq (apply (extractR gr) (Ctxt (cm1,c))) (apply (extractR gr) (Ctxt (cm2,c)))
                                         
  crushT gt = translate $ \ (Ctxt (cm,c)) -> case cm of                     
                                               (Assign v e)  -> apply (extractT gt) (Ctxt (e,c))
                                               (Seq cm1 cm2) -> liftM2 mappend (apply (extractT gt) (Ctxt (cm1,c))) (apply (extractT gt) (Ctxt (cm2,c)))
