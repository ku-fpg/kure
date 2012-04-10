{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module ExprKURE where

import Control.Monad
import Data.Copointed
import Data.Monoid

import Language.KURE
import ExprLanguage


data GenericExpr = GExpr Expr
                 | GCmd Cmd

instance Term GenericExpr where
  type Generic GenericExpr = GenericExpr
  inject  = id
  retract = Just


instance Term Expr where
  
  type Generic Expr = GenericExpr
  
  inject = GExpr
  
  retract (GExpr e) = Just e
  retract _         = Nothing
  
  allR gr = rewrite $ \ c e -> case e of
                                 Lit n     -> pure (Lit n)
                                 Var v     -> pure (Var v)
                                 Add e1 e2 -> liftA2 Add  (apply (extractR gr) c e1) (apply (extractR gr) c e2)
                                 ESeq cm e -> liftA2 ESeq (apply (extractR gr) c cm) (apply (extractR gr) c e)
                                         
  crushT gt = translate $ \ c e -> case e of                     
                                     Lit n     -> pure mempty
                                     Var v     -> pure mempty
                                     Add e1 e2 -> liftA2 mappend (apply (extractT gt) c e1) (apply (extractT gt) c e2)
                                     ESeq cm e -> liftA2 mappend (apply (extractT gt) c cm) (apply (extractT gt) c e)
  
  
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
