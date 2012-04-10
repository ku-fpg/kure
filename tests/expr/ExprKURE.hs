{-# LANGUAGE TypeFamilies #-}

module ExprKURE where

import Control.Applicative
import Data.Monoid

import Language.KURE
import ExprLanguage


data GenericExpr = GExpr Expr
                 | GCmd Cmd

instance Injection GenericExpr GenericExpr where
  inject  = id
  retract = Just

instance Term GenericExpr where
  type Generic GenericExpr = GenericExpr
  

instance Injection Expr GenericExpr where
  
  inject = GExpr
  
  retract (GExpr e) = Just e
  retract _         = Nothing

instance Term Expr where
  
  type Generic Expr = GenericExpr
  
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
  
  allR gr = rewrite $ \ c cm -> case cm of
                                  Assign v e  -> liftA (Assign v) (apply (extractR gr) c e)
                                  Seq cm1 cm2 -> liftA2 Seq (apply (extractR gr) c cm1) (apply (extractR gr) c cm2)
                                         
  crushT gt = translate $ \ c cm -> case cm of                     
                                      Assign v e  -> apply (extractT gt) c e
                                      Seq cm1 cm2 -> liftA2 mappend (apply (extractT gt) c cm1) (apply (extractT gt) c cm2)
