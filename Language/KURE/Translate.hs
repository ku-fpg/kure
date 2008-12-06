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
-- Rewrite (defined in 'Language.KURE.Rewrite') is a synonoym for a 'Translate' with the same source and target type.

module Language.KURE.Translate 
	( Translate
	, apply
	, runTranslate
	, translate
	, translateTransparently
	) where
		

import Control.Monad
import Data.Monoid
import Control.Applicative hiding (many)
import Data.Tree

import Language.KURE.RewriteMonad

-- | 'Translate' is a translation or strategy that translates between @exp1@ and @exp2@, with the posiblity of failure,
-- and remembers identity translations.

newtype Translate m dec exp1 exp2 =
    Translate { applyTranslate :: dec -> exp1 -> RewriteM m dec exp2 }

-- | 'apply' directly applies a 'Translate' value to an argument.
apply :: Translate m dec exp1 exp2 -> dec -> exp1 -> RewriteM m dec exp2
apply = applyTranslate

-- INTERNAL: 'translateWith' is our primitive way of building a Translate. It requires the invoker to declare what is
-- going to happen with the status, typically to handle identity matching.
translateWith 
	:: (Monoid dec, Monad m) 
	=> (RewriteStatusM dec exp2 -> RewriteStatusM dec exp2) 
	-> (dec -> exp1 -> RewriteM m dec exp2) 
	-> Translate m dec exp1 exp2
translateWith f rr = Translate $ \ d e -> updateStatus f (rr d e)

-- | 'translate' is the standard way of building a 'Translate', where if the translation is successful it 
-- is automatically marked as a non-identity translation. 
--
-- @translate $ \ _ e -> return e@ /is not/ an identity rewrite. 
translate :: (Monoid dec, Monad m) => (dec -> exp1 -> RewriteM m dec exp2) -> Translate m dec exp1 exp2
translate = translateWith $ \ r ->
	case r of
	  RewriteReturnM e -> RewriteSuccessM e mempty  -- mark any valid return as a success
	  _ -> r

-- | 'translateTransparently' builds a composite 'Translate', where the @id@ status of the rewrite
-- is determined by the internals calls to apply.
--
-- @translateTransparently $ \ _ e -> return e@ /is/ an identity rewrite. 

translateTransparently
	:: (Monoid dec, Monad m) 
	=> (dec -> exp1 -> RewriteM m dec exp2) 
	-> Translate m dec exp1 exp2
translateTransparently = translateWith id

-- | 'runTranslate' executes the translation, returning either a failure message,
-- or a success and the new parts of the environment.

runTranslate :: (Monoid dec,Monad m) 
	   => Translate m dec exp res
	   -> dec 
	   -> exp 
	   -> m (Either String (res,dec))
runTranslate rr decs exp = do
  res <- runRewriteM (apply rr decs exp)
  case res of
     RewriteSuccessM exp' ds -> return (Right (exp',ds))
     RewriteReturnM exp'     -> return (Right (exp',mempty))
     RewriteFailureM msg     -> return (Left msg)

