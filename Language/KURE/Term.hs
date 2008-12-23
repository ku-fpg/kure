{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-- | This module supports the generic walking of 'Term's. 
--
-- The key idea here is that for each type of expression (@exp@), 
-- we have a sum of all the interesting children types (@Generic exp@).
-- There is always a type that its own 'Generic', which is used for the 
-- deeper syntax tree walks.

module Language.KURE.Term 
	( Term(..)
	, Walker(..)
	, extractR
	, promoteR
	, extractU
	, topdownR
	, bottomupR 
	, alltdR 
	, downupR 
	, innermostR 
	, foldU 
	) where
	
import Language.KURE.RewriteMonad as M	
import Language.KURE.Translate	
import Language.KURE.Rewrite
import Language.KURE.Combinators -- perhaps

import Control.Monad
import Data.Monoid

-- | 'Term's are things that syntax are built from.
class Term exp where
  -- | 'Generic' is a sum of all the interesting sub-types, transitively, of @exp@. 
  -- We use @Generic e ~ e@ to signify that something is its own Generic.
  -- Simple expression types might be their own sole 'Generic', more complex examples
  -- will have a new datatype for the 'Generic', which will also be an instance of class 'Term'.
  type Generic exp

  -- | 'project' projects into a 'Generic' exp, to get the exp inside, or fail.
  -- TODO: rename as select
  select :: Generic exp -> Maybe exp

  -- | 'inject' injects an exp into a 'Generic' exp.
  inject  :: exp -> Generic exp


-- | 'Walker' captures how we walk over @exp@, using a specific @m@ and @dec@.
class (Monoid dec,Monad m,Term exp) => Walker m dec exp where
  -- | 'allR' applies 'Generic' rewrites to all the interesting children of this node.
  allR :: Rewrite m dec (Generic exp) -> Rewrite m dec exp
  -- | 'allU' applied a 'Generic' Translation to a common, 'Monoid'al result, to all the interesting children of this node.
  allU :: (Monoid result) => Translate m dec (Generic exp) result -> Translate m dec exp result

------------------------------------------------------------------------------

-- | 'extract' converts a 'Rewrite' over a 'Generic' into a rewrite over a specific expression type. 

extractR  :: (Monad m, Term exp, Monoid dec) => Rewrite m dec (Generic exp) -> Rewrite m dec exp	-- at *this* type
extractR rr = rewrite $ \ e -> transparently $ do
            e' <- apply rr (inject e)
            case select e' of
                Nothing -> fail "extractR"
                Just r -> return r
                
-- | 'promote' promotes a 'Rewrite' into a 'Generic' 'Rewrite'; other types inside Generic cause failure.
-- 'try' can be used to convert a failure-by-default promotion into a 'id-by-default' promotion.

promoteR  :: (Monad m, Term exp, Monoid dec) => Rewrite m dec exp -> Rewrite m dec (Generic exp)
promoteR rr = rewrite $ \ e -> transparently $ do
               case select e of
                 Nothing -> fail "promoteR"
                 Just e' -> do
                    r <- apply rr e'
                    return (inject r)

-- | 'accept' 


extractU  :: (Monad m, Term exp, Monoid dec) => Translate m dec (Generic exp) r -> Translate m dec exp r
extractU rr = translate $ \ e -> transparently $ apply rr (inject e)


-------------------------------------------------------------------------------

-- apply a rewrite in a top down manner.
topdownR :: (e ~ Generic e, Walker m dec e) => Rewrite m dec e -> Rewrite m dec e
topdownR  s = s >-> allR (topdownR s)

-- apply a rewrite in a bottom up manner.
bottomupR :: (e ~ Generic e, Walker m dec e) => Rewrite m dec e -> Rewrite m dec e
bottomupR s = allR (bottomupR s) >-> s

-- apply a rewrite in a top down manner, prunning at successful rewrites.
alltdR :: (e ~ Generic e, Walker m dec e) => Rewrite m dec e -> Rewrite m dec e
alltdR    s = s <+ allR (alltdR s)

-- apply a rewrite twice, in a topdown and bottom up way, using one single tree traversal.
downupR :: (e ~ Generic e, Walker m dec e) => Rewrite m dec e -> Rewrite m dec e
downupR   s = s >-> allR (downupR s) >-> s

-- a fixed point traveral, starting with the innermost term.
innermostR :: (e ~ Generic e, Walker m dec e) => Rewrite m dec e -> Rewrite m dec e
innermostR s = bottomupR (tryR (s >-> innermostR s))  

-- fold a tree using a single translation for each node.
foldU :: (e ~ Generic e, Walker m dec e, Monoid r) => Translate m dec e r -> Translate m dec e r
foldU s = concatT [ s, allU (foldU s) ]

-------------------------------------------------------------------------------
