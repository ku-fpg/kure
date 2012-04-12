{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module ExprKURE where

import Control.Applicative
import Control.Arrow
import Data.Monoid
import Data.Pointed

import Language.KURE
import ExprLanguage


data GenericExpr = GExpr Expr
                 | GCmd Cmd

instance Term GenericExpr where
  type Generic GenericExpr = GenericExpr
  
instance Walker Context Maybe GenericExpr where
  
  allR r = rewrite $ \ c g -> case g of
                                GExpr e   -> liftA GExpr (apply (allR r) c e)
                                GCmd  cm  -> liftA GCmd  (apply (allR r) c cm)

  crushT t = translate $ \ c g -> case g of 
                                    GExpr e   -> apply (crushT t) c e
                                    GCmd  cm  -> apply (crushT t) c cm
                                    
  chooseL n = lens $ \ c g -> case g of
                                GExpr e  ->  liftA (second (liftA GExpr .)) (apply (chooseL n) c e)
                                GCmd cm  ->  liftA (second (liftA GCmd .))  (apply (chooseL n) c cm)
                              

instance Injection Expr GenericExpr where  
  inject = GExpr
  
  retract (GExpr e) = Just e
  retract _         = Nothing

instance Term Expr where  
  type Generic Expr = GenericExpr
  
instance Walker Context Maybe Expr where
  
  allR r = rewrite $ \ c e -> case e of
                                 Lit n     -> pure (Lit n)
                                 Var v     -> pure (Var v)
                                 Add e1 e2 -> liftA2 Add  (apply (extractR r) c e1) 
                                                          (apply (extractR r) c e2)
                                 ESeq cm e -> liftA2 ESeq (apply (extractR r) c cm) 
                                                          (apply (extractR r) (updateContext cm c) e)
                                         
  crushT t = translate $ \ c e -> case e of                     
                                     Lit n     -> pure mempty
                                     Var v     -> pure mempty
                                     Add e1 e2 -> liftA2 mappend (apply (extractT t) c e1) 
                                                                 (apply (extractT t) c e2)
                                     ESeq cm e -> liftA2 mappend (apply (extractT t) c cm) 
                                                                 (apply (extractT t) (updateContext cm c) e)
  
  chooseL n = lens $ \ c e -> case e of
                                Lit n      ->  empty
                                Var v      ->  empty
                                Add e1 e2  ->  case n of
                                                 0 -> pure ((c,GExpr e1), retractWithA (flip Add e2))
                                                 1 -> pure ((c,GExpr e2), retractWithA (Add e1))
                                                 _ -> empty
                                ESeq cm e  ->  case n of
                                                   0 -> pure ((c,                    GCmd cm), retractWithA (flip ESeq e))
                                                   1 -> pure (((updateContext cm c), GExpr e), retractWithA (ESeq cm))
                                                   _ -> empty

  
instance Injection Cmd GenericExpr where  
  inject = GCmd
  
  retract (GCmd c) = Just c
  retract _        = Nothing
  
instance Term Cmd where  
  type Generic Cmd = GenericExpr
  
instance Walker Context Maybe Cmd where
  
  allR r = rewrite $ \ c cm -> case cm of
                                 Assign v e  -> liftA (Assign v) (apply (extractR r) c e)
                                 Seq cm1 cm2 -> liftA2 Seq (apply (extractR r) c cm1) 
                                                           (apply (extractR r) (updateContext cm1 c) cm2)
                                         
  crushT t = translate $ \ c cm -> case cm of                     
                                      Assign v e  -> apply (extractT t) c e
                                      Seq cm1 cm2 -> liftA2 mappend (apply (extractT t) c cm1) 
                                                                    (apply (extractT t) (updateContext cm1 c) cm2)

  chooseL n = lens $ \ c cm -> case cm of
                                 Assign v e  ->  case n of
                                                   0 -> pure ((c,GExpr e), retractWithA (Assign v))
                                                   _ -> empty
                                 Seq cm1 cm2 ->  case n of
                                                   0 -> pure ((c,GCmd cm1), retractWithA (flip Seq cm2))
                                                   1 -> pure (((updateContext cm1 c), GCmd cm2), retractWithA (Seq cm1))
                                                   _ -> empty
