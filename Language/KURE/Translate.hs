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

-- TODO: make the path explicit inside the Rewrite, and take it out of the monad.

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
	  RewriteReturnM e -> RewriteSuccessM e mempty  -- mark any valid return as a success
	  _ -> r

rewrite :: (Monoid dec, Monad m) => (exp1 -> RewriteM m dec exp1) -> Rewrite m dec exp1
rewrite = translate

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

idRewrite :: (Monad m) =>  Rewrite m dec exp
idRewrite =  Translate $ \ e -> RewriteM $ \ _ _ -> return $ RewriteReturnM e

idR :: (Monad m) =>  Rewrite m dec exp
idR = idRewrite

failTranslate :: (Monad m) => String -> Translate m dec a b
failTranslate msg = Translate $ \ e -> RewriteM $ \ _ _ -> return $ RewriteFailureM msg

failRewrite :: (Monad m) => String -> Rewrite m dec a
failRewrite = failTranslate

class (Monoid dec) => Decs dec where
  type Key dec
  type Dec dec
  lookupDecs :: Key dec -> dec -> Maybe (Dec dec)
  unitDec    :: Key dec -> Dec dec -> dec

instance Decs () where
  type Key () = ()
  type Dec () = ()
  lookupDecs () () = Just ()
  unitDec () () = ()

-----

-- should really use the getDecM method, somehow
getDecs :: (Monad m) => (dec -> Rewrite m dec a) -> Rewrite m dec a
getDecs f = Translate $ \ e -> RewriteM $ \ path dec -> do
	runRewriteM (apply (f dec) e) path dec

-- Going meta on us; .
updateDecs :: (Monoid dec,Monad m) => Rewrite m dec dec -> Rewrite m dec a -> Rewrite m dec a
updateDecs decRR rr = Translate $ \ e -> do
	dec <- getDecsM
	dec' <- apply decRR dec
	setDecsM  dec' $ apply rr e

getPath :: (Monad m)  => (Path -> Rewrite m dec a) -> Rewrite m dec a
getPath f = Translate $ \ e -> RewriteM $ \ path dec -> do
	runRewriteM (apply (f path) e) path dec

applyN :: (Monad m) => Int -> Translate m dec exp1 exp2 -> exp1 -> RewriteM m dec exp2
applyN n rr e = addPathM n $ apply rr e
