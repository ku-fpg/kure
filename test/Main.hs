{-# LANGUAGE ExistentialQuantification, Rank2Types, TypeFamilies, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Main where

import Language.KURE
import Language.KURE.Term as T

import Data.Monoid
import Control.Monad
import Data.List

type Name = String
data Exp = Lam Name Exp
         | App Exp Exp
         | Var Name
   deriving Show


------------------------------------------------------------------------
instance Term Exp where
  type Generic Exp = Exp  -- Exp is its own Generic root.
  inject    = id
  project e = return e

class (Monoid dec) => ExpDec dec where
  addVarBind :: Name -> dec -> Maybe dec
  lookupVarBind :: Name -> dec -> Maybe (Maybe Exp)

------------------------------------------------------------------------
-- First we have the monadic walkers
appM 	:: (Monoid dec, Monad m)
     	=> (a1 -> a2 -> res)
     	-> Translate m dec Exp a1 
	-> Translate m dec Exp a2 
	->  Exp -> RewriteM m dec res
appM f rr1 rr2 (App e1 e2) = do e1' <- apply rr1 e1
			        e2' <- apply rr2 e2
			        return $ f e1' e2'
appM f rr1 rr2 _ = fail "appM"

lamM :: (ExpDec dec, Monoid dec, Monad m)
     	=> (Name -> a1 -> res)
     	-> Translate m dec Exp a1 
	-> Exp -> RewriteM m dec res
lamM f rr1 (Lam n e1) = do
        dec <- getDecsM
	case addVarBind n dec of 
	  Nothing   -> fail "lamR: binding failure"
	  Just env' -> do 
		e1' <- mapDecsM (\ _ -> env') 
		                (apply rr1 e1)
		return $ f n e1'		    
---

appR :: (Monoid dec, Monad m) => Rewrite m dec Exp -> Rewrite m dec Exp -> Rewrite m dec Exp
appR rr1 rr2 = translate (congruenceM . appM App rr1 rr2)

lamR :: (Monad m,ExpDec dec) => Rewrite m dec Exp -> Rewrite m dec Exp
lamR rr = translate (lamM Lam rr)

varR :: (Monoid dec, Monad m) => Rewrite m dec Exp
varR = acceptR (\ e -> case e of
		    Var _ -> True
		    _ -> False)

---
-- Then the guards

appG :: (Monad m, Monoid dec) => Rewrite m dec Exp
appG = appR idR idR

lamG :: (Monad m,ExpDec dec) => Rewrite m dec Exp
lamG = lamR idR

varG :: (Monoid dec, Monad m) => Rewrite m dec Exp
varG = varR

---

varP :: (Monad m, Monoid dec) => (Name -> Translate m dec Exp res) -> Translate m dec Exp res
varP f = varG >-> readerT (\ (Var v) -> f v)

---

appU :: (Monoid dec, Monad m,Monoid res) => Translate m dec Exp res -> Translate m dec Exp res -> Translate m dec Exp res
appU rr1 rr2 = translate (appM (\ a b -> a `mappend` b) rr1 rr2)

lamU :: (Monoid dec, Monad m, ExpDec dec) => Translate m dec Exp res -> Translate m dec Exp res
lamU rr = translate (lamM (\ a b -> b) rr)

varU :: (Monoid dec, Monad m,Monoid ret) => Translate m dec Exp ret
varU = varR >-> translate (\ _ -> congruenceM $ return mempty)

---

instance (Monad m,Monoid dec,ExpDec dec) => Walker m dec Exp where
   allR rr = appR rr rr <+ lamR rr <+ varR
   allU rr = appU rr rr <+ lamU rr <+ varU

--------
{-
freeVar :: (ExpDec dec) => dec -> Name -> Bool
freeVar env nm = case lookupVarBind nm 
-}

-- Perhaps should not be exported here as a trans?

freeExp :: (Walker m dec Exp,ExpDec dec) => Translate m dec Exp [Name]
freeExp = mapDecsT clear frees >-> pureT (Data.List.nub)
   where
	clear _ = mempty
	varFree = varG >-> translate (\ (Var v) -> do
	                env <- getDecsM
			case lookupVarBind v env of
		 	  Nothing -> return [v]
			  Just _ -> return []) 
	frees = varFree <+ allU frees

----

data DecX = DecX [(Name,Maybe Exp)]

instance Monoid DecX where
  mempty = DecX []
  mappend (DecX ab) (DecX cd) = DecX $ ab ++ cd

instance ExpDec DecX where 
   addVarBind v (DecX bds) = Just $  DecX ((v,Nothing) : bds)
   lookupVarBind v (DecX bds) = lookup v bds 

--  addVarBind :: Name -> dec -> Maybe dec
--  lookupVarBind :: Name -> dec -> Maybe Name

----


e1 = Var "x"
e2 = Var "y"
e3 = Lam "x" e1
e4 = Lam "x" e2
e5 = App e1 e2
e6 = App e3 e4
e7 = App e4 e6

main = do
	let es1 = [e1,e2,e3,e4,e5,e6,e7]
	sequence_ [ print e | e <- es1]

	let frees :: Exp -> IO [Name]
	    frees exp = do Right (fs,b) <- runTranslate freeExp (mempty :: DecX) exp
			   return fs
	e_frees <- mapM frees es1
	sequence_ [ print e | e <- e_frees]
	

{-
freeExp = (varG >-> 
	   varG >-> (translate $ \ env (Var v) -> return [v])
	<+ 
	translate $ \ env (Lam v e) -> 

	lamU freeExp >-> 
	<+ varU freeExp freeExp
    
-}	
	

{-
freeExp :: Translate m Decx Exp [Name]
freeExp = translate fn
  where
	fn (Lam n e)   = apply freeExp e >=> (remove n)
	fn (App e1 e2) = all freeExp 
-}
{-
class Monad m => NameSupply m where
   newName :: m Name

freeExp :: Exp -> [Name]
freeExp = undefined

subst :: (Monoid dec, NameSupply m, Walker m dec Exp) => Name -> Exp -> Rewrite m dec Exp
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
instance Monoid DecX where
  type Key DecX = Name
  type Dec DecX = Maybe Exp
  lookupMonoid nm (DecX decx) = lookup nm decx
  unitDec nm val = DecX [(nm,val)]
-}

instance ExpDec DecX where
   addVarBind nm (DecX dec) = case lookup nm dec of
			   Nothing -> return $ DecX ((nm,Nothing) : dec)
			   Just env -> fail "binding name clash"
{-			
subst' :: (ExpDec dec, Monoid dec, NameSupply m, Walker m dec Exp) => Name -> Exp -> Rewrite m dec Exp
subst' n exp = updateMonoid rrEnv >-> sub 
  where
	rrEnv = idR

	sub = translate inlineN
	      lamR sub <+
	      appR sub sub

inlineN n (Var n') | n == n' = 
-}

{-
	n = undefined
	e = undefined



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
-}


eval :: (Monoid dec, NameSupply m, Walker m dec Exp) => Rewrite m dec Exp
eval = 
    translate (\ e' -> case e' of
	(App (Lam v e1) e2) -> apply (subst v e2) e1  -- beta reduction
	_ -> fail "") <+
    T.all eval

-}