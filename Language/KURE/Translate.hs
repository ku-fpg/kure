-- |
-- Module: Language.KURE.Translate 
-- Copyright: (c) 2006-2008 Andy Gill
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- 'Translate' is the main abstraction inside KURE, and represents a rewriting from a source to a target
-- of a possibly different type.
--
-- 'Rewrite' (defined in Language.KURE.Translate) is a synonoym for a 'Translate' with the same source and target type.

module Language.KURE.Translate 
	( Translate
	, apply
	, runTranslate
	, translate
	, translateWith
	, translateWithId
	, failT
	, getDecsT
	, mapDecsT
	, mapT
	) where
		

import Control.Monad
import Data.Monoid
import Control.Applicative hiding (many)
import Data.Tree

import Language.KURE.RewriteMonad

-- abstract
newtype Translate m dec exp1 exp2 =
    Translate { applyTranslate :: dec -> exp1 -> RewriteM m dec exp2 }

-- | 'apply' directly applies a translate to arguments.
apply :: Translate m dec exp1 exp2 -> dec -> exp1 -> RewriteM m dec exp2
apply = applyTranslate


-- | 'translateWith' is our primitve way of building a Translate. It requires the invoker to declare what is
-- going to happen with the status, typically to handle identity matching.
translateWith 
	:: (Monoid dec, Monad m) 
	=> (RewriteStatusM dec exp2 -> RewriteStatusM dec exp2) 
	-> (dec -> exp1 -> RewriteM m dec exp2) 
	-> Translate m dec exp1 exp2
translateWith f rr = Translate $ \ d e -> updateStatus f (rr d e)

-- 'translate' is the standard way of building a 'Translate', where if the translation is successful it 
-- is marked as non-identity.

translate :: (Monoid dec, Monad m) => (dec -> exp1 -> RewriteM m dec exp2) -> Translate m dec exp1 exp2
translate = translateWith $ \ r ->
	case r of
	  RewriteReturnM e -> RewriteSuccessM e mempty  -- mark any valid return as a success
	  _ -> r

translateWithId
	:: (Monoid dec, Monad m) 
	=> (dec -> exp1 -> RewriteM m dec exp2) 
	-> Translate m dec exp1 exp2
translateWithId = translateWith id


runTranslate :: (Monoid dec,Monad m) 
	   => Translate m dec exp res
	   -> dec 
	   -> exp 
	   -> m (res,dec)
runTranslate rr decs exp = do
  res <- runRewriteM (apply rr decs exp)
  case res of
     RewriteSuccessM exp' ds -> return (exp',ds)
     RewriteReturnM exp'     -> return (exp',mempty)
     RewriteFailureM msg     -> fail msg

failT :: (Monad m) => String -> Translate m dec a b
failT msg = Translate $ \ dec e -> failM msg


getDecsT :: (Monad m) => (dec -> Translate m dec a b) -> Translate m dec a b
getDecsT f = Translate $ \ dec e -> apply (f dec) dec e

-- Going meta on us; 
mapDecsT :: (Monoid dec,Monad m) => (dec -> RewriteM m dec dec) -> Translate m dec a r -> Translate m dec a r
mapDecsT f_env rr = Translate $ \ env e -> do
	env' <- f_env env
	apply rr env' e


mapT :: (Monad m,Monoid dec) => (a -> b) -> Translate m dec a b
mapT f = translate $ \ env a -> return (f a)

