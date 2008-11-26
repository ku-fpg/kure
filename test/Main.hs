{-# LANGUAGE ExistentialQuantification, Rank2Types, TypeFamilies, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Main where

import Language.KURE.Rewrite 
import Language.KURE.Translate
import Language.KURE.Combinators
import Language.KURE.Term as T
import Data.Monoid
import Control.Monad

main  = print "Hello"

type Name = String
data Exp = Lam Name Exp
         | App Exp Exp
         | Var Name

data DecX = DecX [(Name,Maybe Exp)]
instance Monoid DecX where {}

------------------------------------------------------------------------
-- First we have the monadic walkers
appM 	:: (Monoid dec, Monad m)
     	=> (a1 -> a2 -> res)
     	-> Translate m dec Exp a1 
	-> Translate m dec Exp a2 
	-> Exp -> RewriteM m dec res
appM f rr1 rr2 (App e1 e2) = do e1' <- apply rr1 e1
			        e2' <- apply rr2 e2
			        return $ f e1' e2'
appM f rr1 rr2 _ = fail "appM"


appR :: (Monoid dec, Monad m) => Rewrite m dec Exp -> Rewrite m dec Exp -> Rewrite m dec Exp
appR rr1 rr2 = rebuild (appM App rr1 rr2)

appQ :: (Monad m, Monoid dec) => Rewrite m dec Exp
appQ = appR idR idR

appU :: (Monoid dec, Monad m,Monoid res) => Translate m dec Exp res -> Translate m dec Exp res -> Translate m dec Exp res
appU rr1 rr2 = translate (appM (\ a b -> a `mappend` b) rr1 rr2)

lamR :: (Monad m,ExpDec dec) => Rewrite m dec Exp -> Rewrite m dec Exp
lamR rr = rebuild (\ e -> case e of
		Lam n e1 -> do env <- getDecsM
			       case addVarBind n env of 
				 Nothing   -> fail "lamR: binding failure"
				 Just env' -> do 
			       		e1' <- setDecsM env $ apply rr e1
			       		return $ Lam n e1'
		_ -> fail "lamR")

lamU :: (Monad m,ExpDec dec) => Translate m dec Exp ret -> Translate m dec Exp ret
lamU rr = translate (\ e -> case e of
		Lam n e1 -> do env <- getDecsM
			       case addVarBind n env of 
				 Nothing   -> fail "lamU: binding failure"
				 Just env' -> do 
			       		e1' <- setDecsM env $ apply rr e1
			       		return $ e1'
		_ -> fail "lamR")

varR :: (Decs dec, Monad m) => Rewrite m dec Exp
varR = accept (\ e -> case e of
		    Var _ -> True
		    _ -> False)

varU :: (Decs dec, Monad m,Monoid ret) => Translate m dec Exp ret
varU = translate (\ e -> case e of
		    Var _ -> return mempty
		    _ -> fail "varU")

class (Monoid dec) => ExpDec dec where
  addVarBind :: Name -> dec -> Maybe dec 

instance (Monad m,Decs dec,ExpDec dec) => Walker' m dec Exp where
   allR rr = appR rr rr <+ lamR rr <+ varR
   allU rr = appU rr rr <+ lamU rr <+ varU

------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------



data Bind = LAM


-- Exp is its own Generic.
instance Term Exp where
  type Generic Exp = Exp
  inject    = id
  project e = return e

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

subst :: (Decs dec, NameSupply m, Walker m dec Exp) => Name -> Exp -> Rewrite m dec Exp
subst n e = 
	rewrite rrRule1 <+
	accept  isRule2 <+
	accept  isRule3 <+
	rewrite rrRule4 <+
	rewrite rrRule5 <+
	T.all (subst n e)	-- rule 6
  where
	rrRule1 (Var n') | n == n' = return e
	rrRule1 _                  = fail "rule 1"
	
	isRule2 (Var n') = n /= n'
	isRule2 _        = False
		
	isRule3 (Lam n' e') = n == n'
	isRule3 _           = False

	rrRule4 (Lam n' e') 
	   | n `notElem` freeExp e' || n' `notElem` freeExp e
	   = liftM (Lam n') $ apply (subst n e) e'
	rrRule4 _ = fail "rule 4"
	
	rrRule5 (Lam n' e') 
	   | n `elem` freeExp e' && n' `elem` freeExp e
	   = do n'' <- liftQ newName
		liftM (Lam n'') $ apply (subst n' (Var n'') >-> subst n e) e'

{-
clashQ :: Translate m dec Exp a
clashQ = reader (\ e -> case e of
	    Lam v1 e2 -> environment (\ env -> 
			     
	    _ -> error "clashQ fail"
-}
    

-- assumes all bindings are unique.
{-
instance Decs DecX where
  type Key DecX = Name
  type Dec DecX = Maybe Exp
  lookupDecs nm (DecX decx) = lookup nm decx
  unitDec nm val = DecX [(nm,val)]
-}

instance ExpDec DecX where
   addVarBind nm (DecX dec) = case lookup nm dec of
			   Nothing -> return $ DecX ((nm,Nothing) : dec)
			   Just env -> fail "binding name clash"
			
subst' :: (ExpDec dec, Decs dec, NameSupply m, Walker m dec Exp) => Rewrite m dec Exp -> Rewrite m dec Exp
subst' rr =
	rr <+ -- rule 1 and 2 (subst' rr) <+
	lamR (subst' rr) <+
	appR (subst' rr) (subst' rr)
  where
	n = undefined
	e = undefined
{-
	rrRule1 (Var n') | n == n' = return e
	rrRule1 _                  = fail "rule 1"
	
	isRule2 (Var n') = n /= n'
	isRule2 _        = False
-}
		
	isRule3 (Lam n' e') = n == n'
	isRule3 _           = False

	rrRule4 (Lam n' e') 
	   | n `notElem` freeExp e' || n' `notElem` freeExp e
	   = liftM (Lam n') $ apply (subst n e) e'
	rrRule4 _ = fail "rule 4"
	
	rrRule5 (Lam n' e') 
	   | n `elem` freeExp e' && n' `elem` freeExp e
	   = do n'' <- liftQ newName
		liftM (Lam n'') $ apply (subst n' (Var n'') >-> subst n e) e'



eval :: (Decs dec, NameSupply m, Walker m dec Exp) => Rewrite m dec Exp
eval = 
    translate (\ e' -> case e' of
	(App (Lam v e1) e2) -> apply (subst v e2) e1  -- beta reduction
	_ -> fail "") <+
    T.all eval


