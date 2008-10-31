{-# LANGUAGE ExistentialQuantification, Rank2Types, TypeFamilies #-}

module Main where


import Language.KURE.Rewrite 

main  = print "Hello"

type Name = String
data Exp = Lam Name Exp
         | App Exp Exp
         | Var Name

data Decs = Decs

data MySubstEnv i = MySubstEnv { me :: forall m . Rewrite m i Decs Exp }

instance Subst Exp where
  substInsideNode (Var name) = Cons Var :. name

--  thisSubstRewrite :: SubstEnv i -> Rewrite m i d s  
