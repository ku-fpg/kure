{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-- | This module supports the generic walking of 'Term's. 


module Language.KURE.Term 
	( Term(..)
	, Walker(..)
	, extract
	, promote
	) where
	
import Language.KURE.RewriteMonad as M	
import Language.KURE.Translate	
import Language.KURE.Rewrite
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

topdownR  s = s >-> allR (topdownR s)
bottomupR s = allR (bottomupR s) >-> s
alltdR    s = s <+ allR (alltdR s)
downupR   s = s >-> allR (downupR s) >-> s
innermost s = bottomupR (tryR s)  

