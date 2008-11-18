{-# LANGUAGE ExistentialQuantification, TypeFamilies, Rank2Types, MultiParamTypeClasses, FlexibleContexts, GADTs #-}

module Language.KURE.Term where
	
import Language.KURE.Translate	
import Language.KURE.Rewrite as M	
import Language.KURE.Combinators -- perhaps

import Control.Monad
import Data.Monoid

class Term exp where
  type Generic exp
  type Context exp  -- fixed for this *type* of term

--  explodeCons :: exp -> Shape exp

  walkCons :: (Monad m,Monoid (Context exp)) => exp -> Rewrite m (Context exp) (Generic exp) -> RewriteM m (Context exp) exp

  -- everything follows from these
  project :: (Monad m) => Generic exp -> m exp
  inject  :: exp -> Generic exp

------------------------------------------------------------------------------

extract  :: (Term exp, Monad m, Monoid dec, dec ~ Context exp) => Rewrite m dec (Generic exp) -> Rewrite m dec exp	-- at *this* type
extract rr = translateWith id $ \ e -> do
            e' <- apply rr (inject e)
            project e'

-- promote a rewrite into a generic rewrite; other types are fails.
package  :: (Term exp, Monad m, Monoid dec) => Rewrite m dec exp -> Rewrite m dec (Generic exp)
package rr = translateWith id $ \ e -> do
               e' <- project e
               r <- apply rr e'
               return (inject r)

------------------------------------------------------------------------------

data X m dec exp a = X Int  (Rewrite m (Context exp) (Generic exp) -> (RewriteM m dec a))

walkOver :: (Monad m,Monoid dec, dec ~ Context exp) 
	=> X m dec exp exp -> Rewrite m (Context exp) (Generic exp) 
	-> RewriteM m (Context exp) exp
walkOver (X _ m) = m

cons :: (Monad m,Monoid dec) => a -> X m dec exp a 
cons a = X 0 (\ _ -> return a)

infixl 3 `rec`, `keep`

rec 	:: (Monad m ,Monoid dec, Generic exp ~ Generic e1, dec ~ Context e1, dec ~ Context exp, Term e1) 
	=> X m dec exp (e1 -> e2) 
	-> e1
	-> X m dec exp e2
rec (X i m) e = X (succ i) $ \ env -> do 	
	v <- m env
	a <- addPathM i (apply (extract env) e)
	return (v a)

keep :: (Monad m,Monoid dec) => X m dec exp (e1 -> e2) -> e1 -> X m dec exp e2
keep (X i m) a = X (succ i) $ \ env -> do
	v <- m env
	return (v a)

all :: (Term exp, Monad m, Decs dec, dec ~ Context exp) 
       => Rewrite m (Context exp) (Generic exp) 
       -> Rewrite m (Context exp) exp
all rr = translateWith id $ \ e -> walkCons e rr

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
