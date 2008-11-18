{-# LANGUAGE ExistentialQuantification, Rank2Types, TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module Main where

import Language.KURE.Rewrite 
import Language.KURE.Translate
import Language.KURE.Combinators
import Language.KURE.Term as T
import Data.Monoid

main  = print "Hello"

type Name = String
data Exp = Lam Name Exp
         | App Exp Exp
         | Var Name

data Decx = Decx [(Name,Bind)]
data Bind = LAM

instance Monoid Decx where {}

-- Exp is its own Generic.
instance Term Exp where
  type Generic Exp = Exp
  inject    = id
  project e = return e

-- I'm reinventing generics here!
instance Walker Decx Exp where
{-
  walkCons (Lam n e) = walkOver $ cons Lam
	`keep` n
	`recWith` Scope (unitDec n LAM) e
 -}
  walkCons (App e1 e2) = walkOver $ cons App
	`rec` e1
	`rec` e2
  walkCons (Var v) = walkOver $ cons Var
	`keep` v

-- subst :: (Monad m) => Var -> Rewrite m Context 
-- subst v = undefined

{-
freeExp :: Translate m Decx Exp [Name]
freeExp = translate fn
  where
	fn (Lam n e)   = apply freeExp e >=> (remove n)
	fn (App e1 e2) = all freeExp 
-}

class Monad m => NameSupply m where
   newName :: m Name

freeExp :: Exp -> [Name]
freeExp = undefined

subst :: (Decs dec, NameSupply m, Walker dec Exp) => Name -> Exp -> Rewrite m dec Exp
subst n e =
	translate (\ e' -> case e' of
	     Var n'    | n == n'     -> return e
 	     Lam n' e2 | n == n'     -> return e'  -- could be id here??
 	     Lam n' e2 | n' `elem` freeExp e -> do
			n'' <- liftQ newName
			e3 <- apply (subst n' (Var n'') >-> subst n e) e2
			return $ Lam n'' e3
	     _ -> fail "") <+
	T.all (subst n e)  <+ idRewrite

eval :: (Decs dec, NameSupply m, Walker dec Exp) => Rewrite m dec Exp
eval = 
    translate (\ e' -> case e' of
	(App (Lam v e1) e2) -> apply (subst v e2) e1  -- beta reduction
	_ -> fail "") <+
    T.all eval


