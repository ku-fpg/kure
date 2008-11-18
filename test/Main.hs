{-# LANGUAGE ExistentialQuantification, Rank2Types, TypeFamilies #-}

module Main where

import Language.KURE.Rewrite 
import Language.KURE.Translate
import Language.KURE.Combinators
import Language.KURE.Term

main  = print "Hello"

type Name = String
data Exp = Lam Name Exp
         | App Exp Exp
         | Var Name

data Decs = Decs [(Name,Exp)]

-- Exp is its own Generic.

instance Term Exp where
  type Generic Exp = Exp
  type Context Exp = ()

  walkCons (Lam n e) = walkOver $ cons Lam
	`keep` n
	`rec` e
  walkCons (App e1 e2) = walkOver $ cons App
	`keep` e1
	`keep` e2
  walkCons (Var v) = walkOver $ cons Var
	`keep` v

  inject    = id
  project e = return e

-- subst :: (Monad m) => Var -> Rewrite m Context 
-- subst v = undefined

