{-# LANGUAGE ExistentialQuantification, TypeFamilies, Rank2Types, MultiParamTypeClasses, FlexibleContexts, GADTs #-}

module Language.KURE.Term where
	
import Language.KURE.Translate	
import Language.KURE.Rewrite as M	
import Language.KURE.Combinators -- perhaps

import Control.Monad
import Data.Monoid

class Term exp where
  type Generic exp

  project :: (Monad m) => Generic exp -> m exp
  inject  :: exp -> Generic exp

class (Monoid dec,Monad m,Term exp) => Walker m dec exp where
  allR :: Rewrite m dec (Generic exp) -> Rewrite m dec exp
  allU :: (Monoid result) => Translate m dec (Generic exp) result -> Translate m dec exp result

------------------------------------------------------------------------------

extract  :: (Monad m, Term exp, Monoid dec) => Rewrite m dec (Generic exp) -> Rewrite m dec exp	-- at *this* type
extract rr = translateWith id $ \ dec e -> do
            e' <- apply rr dec (inject e)
            project e'

-- promote a rewrite into a generic rewrite; other types are fails.
package  :: (Monad m, Term exp, Monoid dec) => Rewrite m dec exp -> Rewrite m dec (Generic exp)
package rr = translateWith id $ \ dec e -> do
               e' <- project e
               r <- apply rr dec e'
               return (inject r)

------------------------------------------------------------------------------

data X m dec exp a = X Int  (Rewrite m dec (Generic exp) -> (RewriteM m dec a))

walkOver :: (Monad m,Monoid dec) 
	=> X m dec exp exp -> Rewrite m dec (Generic exp) 
	-> RewriteM m dec exp
walkOver (X _ m) = m

cons :: (Monad m,Monoid dec) => a -> X m dec exp a 
cons a = X 0 (\ _ -> return a)

-- infixl 3 `rec`, `keep`, `recWith`
{-
rec 	:: (Monoid dec, Generic exp ~ Generic e1, Walker m dec exp, Walker m dec e1) 
	=> X m dec exp (e1 -> e2) 
	-> e1
	-> X m dec exp e2
rec (X i m) e = X (succ i) $ \ env -> do 	
	v <- m env
	a <- addPathM i (apply (extract env) e)
	return (v a)

recWith :: (Monad m, Monoid dec, Generic exp ~ Generic e1, Walker m dec exp, Walker m dec e1) 
	=> X m dec exp (e1 -> e2) 
	-> ((e1 -> RewriteM m dec e1) -> RewriteM m dec e1)
	-> X m dec exp e2
recWith (X i m) f = X (succ i) $ \ env -> do 	
	v <- m env
	a <- addPathM i (f (apply (extract env)))
	return (v a)

keep :: (Monad m,Monoid dec) => X m dec exp (e1 -> e2) -> e1 -> X m dec exp e2
keep (X i m) a = X (succ i) $ \ env -> do
	v <- m env
	return (v a)

all :: (Walker m dec exp) 
       => Rewrite m dec (Generic exp) 
       -> Rewrite m dec exp
all rr = translateWith id $ \ e -> walkCons e rr


-- I'm reinventing generics here!
instance (Monad m) => Walker m () Exp where
  walkCons (Lam n e) = walkOver $ cons Lam
	`keep` n
	`rec` e
  walkCons (App e1 e2) = walkOver $ cons App
	`rec` e1
	`rec` e2
  walkCons (Var v) = walkOver $ cons Var
	`keep` v

instance (NameSupply m) => Walker m DecX Exp where
  walkCons (Lam n e) = \ env -> do
	n' <- liftQ newName
	flip walkOver env $ cons Lam
          `keep` n'
	  `recWith` (\ app -> updateDecsM (\ dec -> dec) $ app e)
	
  walkCons (App e1 e2) = walkOver $ cons App
	`rec` e1
	`rec` e2
  walkCons (Var v) = walkOver $ cons Var
	`keep` v

--	Scope (unitDec n LAM) e
-- subst :: (Monad m) => Var -> Rewrite m Context 
-- subst v = undefined


-}

------------------------------------------------------------------------------
{-
data Generics
	= GExp Exp
	| GRoot Root

data Exp = Exp Int Exp Root

instance Term Exp where
  type Generic Exp = Generics
  type Context Exp = ()

  walkCons (Exp i e r) = walkOver $ cons Exp
	  `keep` i
	  `rec` e 
	  `rec` r 

{-
  explodeCons (Exp n e r) = Cons Exp 
                            :. n 
                            :* e 
                            :** (Scoped () r)
-}
  inject = GExp

  project (GExp e) = return e
  project _        = fail "project of non-GExp"

data Root = Root Exp

instance Term Root where
  type Generic Root = Generics
  type Context Root = ()
{-
  explodeCons (Root e) = Cons Root :* e 
-}
  inject = GRoot

  project (GRoot e) = return e
  project _         = fail "project of non-GRoot"
-}
