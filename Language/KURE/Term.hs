{-# LANGUAGE ExistentialQuantification, TypeFamilies, Rank2Types, MultiParamTypeClasses, FlexibleContexts, GADTs #-}

module Language.KURE.Term where
	
import Language.KURE.Translate	
import Language.KURE.Rewrite as M	
import Language.KURE.Combinators -- perhaps

import Control.Monad
import Data.Monoid

class Term exp where
  type Generic exp
--  type Context exp  -- fixed for this *type* of term

--  explodeCons :: exp -> Shape exp


  -- everything follows from these
  project :: (Monad m) => Generic exp -> m exp
  inject  :: exp -> Generic exp

class (Monad m,Monoid dec,Term exp) => Walker m dec exp where
  walkCons :: (Monad m,Monoid dec) => exp -> Rewrite m dec (Generic exp)       -> RewriteM m dec exp
  joinCons :: (Monad m,Monoid dec) => exp -> Translate m dec (Generic exp) res -> RewriteM m dec res

class (Monoid dec,Monad m,Term exp) => Walker' m dec exp where
  allR :: Rewrite m dec (Generic exp) -> Rewrite m dec exp
  allU :: (Monoid result) => Translate m dec (Generic exp) result -> Translate m dec exp result

------------------------------------------------------------------------------

extract  :: (Walker m dec exp, Monoid dec) => Rewrite m dec (Generic exp) -> Rewrite m dec exp	-- at *this* type
extract rr = translateWith id $ \ e -> do
            e' <- apply rr (inject e)
            project e'

-- promote a rewrite into a generic rewrite; other types are fails.
package  :: (Walker m dec exp, Monoid dec) => Rewrite m dec exp -> Rewrite m dec (Generic exp)
package rr = translateWith id $ \ e -> do
               e' <- project e
               r <- apply rr e'
               return (inject r)

------------------------------------------------------------------------------

data X m dec exp a = X Int  (Rewrite m dec (Generic exp) -> (RewriteM m dec a))

walkOver :: (Monad m,Monoid dec) 
	=> X m dec exp exp -> Rewrite m dec (Generic exp) 
	-> RewriteM m dec exp
walkOver (X _ m) = m

cons :: (Monad m,Monoid dec) => a -> X m dec exp a 
cons a = X 0 (\ _ -> return a)

infixl 3 `rec`, `keep`, `recWith`

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
