{-# LANGUAGE ExistentialQuantification, TypeFamilies, Rank2Types, MultiParamTypeClasses, FlexibleContexts, GADTs, FlexibleInstances #-}

-- | This module supports the generic walking of 'Term's. 


module Language.KURE.Term 
	( Term(..)
	, Walker(..)
	, extract
	, promote
	) where
	
import Language.KURE.Translate	
import Language.KURE.Rewrite as M	
import Language.KURE.Combinators -- perhaps

import Control.Monad
import Data.Monoid

class Term exp where

  -- | 'Generic' is a sum of all the interesting sub-types, transitively, of @exp@. 
  --  simple expression types might be their own sole 'Generic'.
  type Generic exp
  data GenericBox exp

  inBox :: Generic exp -> GenericBox exp
  outBox :: GenericBox exp -> Generic exp


  -- | 'project' projects into a 'Generic' exp, to get the exp inside, or fail.
  project :: (Monad m) => Generic exp -> m exp

  -- | 'inject' injects an exp into a 'Generic' exp.
  inject  :: exp -> Generic exp

--   allR' :: (Monoid dec, Monad m, Generic exp ~ Generic exp') => Rewrite m dec (Generic exp') -> Rewrite m dec (Generic exp)

class (Monoid dec,Monad m,Term exp) => Walker m dec exp where
  -- | 'allR' applies 'Generic' rewrites to all the interesting children of this node.
  allR :: Rewrite m dec (Generic exp) -> Rewrite m dec exp
  -- | 'allU' applied a 'Generic' Translation to a common, 'Monoid'al result, to all the interesting children of this node.
  allU :: (Monoid result) => Translate m dec (Generic exp) result -> Translate m dec exp result

{-
class IsGeneric exp where 
	
  allX :: Rewrite m dec exp -> Rewrite m dec exp
  allX = id
-}

------------------------------------------------------------------------------

-- | 'extract' converts a 'Rewrite' over a 'Generic' into a rewrite over a specific expression type. 

extract  :: (Monad m, Term exp, Monoid dec) => Rewrite m dec (Generic exp) -> Rewrite m dec exp	-- at *this* type
extract rr = translateWith id $ \ dec e -> do
            e' <- apply rr dec (inject e)
            project e'

-- | 'promote' promotes a 'Rewrite' into a 'Generic' 'Rewrite'; other types inside Generic cause failure.

promote  :: (Monad m, Term exp, Monoid dec) => Rewrite m dec exp -> Rewrite m dec (Generic exp)
promote rr = translateWith id $ \ dec e -> do
               e' <- project e
               r <- apply rr dec e'
               return (inject r)

-------------------------------------------------------------------------------

--topdown :: (Walker m dec exp) => (forall exp' . ( 	Generic exp' ~ Generic exp) => Rewrite m dec exp' -> Rewrite m dec exp') -> Rewrite m dec exp -> Rewrite m dec exp
--topdown :: (Walker m dec exp) => Rewrite m dec (Generic exp) -> Rewrite m dec exp
--topdown s = extract s >-> allX (topdown' s)

--topdown' :: (Walker m dec exp, Generic exp ~ Generic exp') => (foreach exp' . Rewrite m dec (Generic exp') -> Rewrite m dec (Generic exp)
--topdown' s = allX s
-- promote (topdown s)

-- topdown s = allR' (topdown s)

data Exp = Exp

data EXP = EXP

instance Term Exp where 
	type Generic Exp = EXP

instance Term EXP where 
	type Generic EXP = EXP
	
instance (Monad m,Monoid dec) => Walker m dec Exp where {}

-- topdown :: (Monoid dec,Monad m, a ~ EXP, a ~ Generic Exp, Generic Exp ~ EXP,Walker m dec Exp) => Rewrite m dec a -> Rewrite m dec a
--- topdown ::  (Monoid dec, Monad m, IsGeneric e) => Translate m dec e e -> Translate m dec e e
topdown' :: (Walker m dec e, e ~ Generic e) => Translate m dec e e -> Translate m dec e e
topdown' s = s >-> allR (topdown' s) -- undefined -- promote (extract s >-> topdown' s)

topdown :: (Walker m dec e, e ~ Generic e, e ~ Generic e', Term e') => Translate m dec e e -> Translate m dec e' e'
topdown s = extract (topdown' s)

--topdown' :: (Monoid dec,Monad m, a ~ Exp, aa ~ EXP, Walker m dec Exp) => Rewrite m dec aa -> Rewrite m dec a 
--topdown' :: (Monoid dec,Monad m, aa ~ Generic a, Walker m dec a) => Rewrite m dec aa -> Rewrite m dec a 
--topdown' s = undefined -- allX (topdown s)

{-

topdownR' :: (Walker m dec exp) 
 	=> Rewrite m dec (Generic exp) 
	-> Rewrite m dec (Generic exp) 
	-> Rewrite m dec (Generic exp)
topdownR' s r = s >-> promote (allR (topdownR r))

jig :: (Walker m dec exp1, Walker m dec exp2, Generic exp1 ~ Generic exp2) => Rewrite m dec (Generic exp1) -> Rewrite m dec (Generic exp')
jig = undefined

topdownR :: (Walker m dec exp) => Rewrite m dec (Generic exp) -> Rewrite m dec (Generic exp)
topdownR s = undefined -- extract (topdownR' (extract s) s)
--  where
--	ex = extract

-- topdownR2 :: (Rewrite m dec (Generic exp) -> Rewrite m dec (Generic exp)


--	extract s >-> allR (undefined s)
{-
f :: (Walker m dec exp1, Walker m dec exp2, Generic exp1 ~ Generic exp2)
	=> Rewrite m dec (Generic exp2)
	-> Rewrite m dec (Generic exp1)
f s = promote (topdownR s)
-}
-- promote (allR (topdownR s))


-}
