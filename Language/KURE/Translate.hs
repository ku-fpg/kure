{-# LANGUAGE ExistentialQuantification, TypeFamilies, Rank2Types, MultiParamTypeClasses, FlexibleContexts, GADTs #-}
-- |
-- Module: Language.KURE.Translate 
-- Copyright: (c) 2006-2008 Andy Gill
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc
--

module Language.KURE.Translate where
	{-
       	( Rewrite			-- syn
       	, RewriteM			-- abstract
       	, Translate
       	, runRewrite
       	, idRewrite
	, failTranslate
       	, liftQ
       	, rewrite
       	, apply
       	, Decs(..)
       ) where -}

import Control.Monad
import Data.Monoid
import Control.Applicative hiding (many)
import Data.Tree

import Language.KURE.Rewrite

newtype Translate m dec exp1 exp2 =
    Translate { applyTranslate :: exp1 -> RewriteM m dec exp2 }

type Rewrite m dec exp = Translate m dec exp exp

apply :: Translate m dec exp1 exp2 -> exp1 -> RewriteM m dec exp2
apply = applyTranslate

rebuild :: (exp1 -> RewriteM m dec exp1) -> Rewrite m dec exp1
rebuild = Translate

translate :: (Monoid dec, Monad m) => (exp1 -> RewriteM m dec exp2) -> Translate m dec exp1 exp2
translate = translateWith $ \ r ->
	case r of
	  RewriteReturnM e -> RewriteSuccessM e mempty
	  _ -> r
	
translateWith 
	:: (Monoid dec, Monad m) 
	=> (RewriteStatusM dec exp2 -> RewriteStatusM dec exp2) 
	-> (exp1 -> RewriteM m dec exp2) 
	-> Translate m dec exp1 exp2
translateWith f rr = Translate $ \ e -> updateStatus f (rr e)


runRewrite :: (Decs dec,Monad m) =>  Rewrite m dec exp 
	   -> Path 
	   -> dec 
	   -> exp 
	   -> m (exp,dec)
runRewrite rr path decs exp = do
  res <- runRewriteM (apply rr exp) path decs
  case res of
     RewriteSuccessM exp' ds -> return (exp',ds)
     RewriteReturnM exp             -> return (exp,mempty)
     RewriteFailureM msg        -> fail msg

idRewrite :: (Decs dec,Monad m) =>  Rewrite m dec exp
idRewrite =  Translate $ \ e -> RewriteM $ \ _ _ -> return $ RewriteReturnM e

failTranslate :: (Monad m) => String -> Translate m dec a b
failTranslate msg = Translate $ \ e -> RewriteM $ \ _ _ -> return $ RewriteFailureM msg

class (Monoid dec) => Decs dec where
  type Key dec
  type Dec dec
  lookupDecs :: Key dec -> dec -> Dec dec
  unitDec    :: Key dec -> Dec dec -> dec

instance Decs () where
  type Key () = ()
  type Dec () = ()
  lookupDecs () () = ()
  unitDec () () = ()
