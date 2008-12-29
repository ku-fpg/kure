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
	) where
		
import Control.Monad
import Data.Monoid

import Language.KURE.RewriteMonad

-- | 'Translate' is a translation or strategy that translates between @exp1@ and @exp2@, with the posiblity of failure,
-- and remembers identity translations.

newtype Translate m dec exp1 exp2 =
    Translate ( exp1 -> RewriteM m dec exp2 )

-- | 'apply' directly applies a 'Translate' value to an argument.
apply :: (Monoid dec, Monad m) => Translate m dec exp1 exp2 -> exp1 -> RewriteM m dec exp2
apply (Translate t) exp1 = t exp1 


-- | 'translate' is the standard way of building a 'Translate', where if the translation is successful it 
-- is automatically marked as a non-identity translation. 
--
-- Note: @translate $ \ _ e -> return e@ /is not/ an identity rewrite, but a succesful rewrite that
-- returns its provided argument. 

translate :: (Monoid dec, Monad m) => (exp1 -> RewriteM m dec exp2) -> Translate m dec exp1 exp2
translate f = Translate $ \ e -> markM $ f e


-- | 'runTranslate' executes the translation, returning either a failure message,
-- or a success and the new parts of the environment.

runTranslate :: (Monoid dec,Monad m) 
	   => Translate m dec exp res
	   -> dec 
	   -> exp 
	   -> m (Either String (res,dec))
runTranslate rr dec e = do
  res <- runRewriteM (apply rr e) dec
  case res of
     RewriteReturnM exp' Nothing _   -> return (Right (exp',mempty))
     RewriteReturnM exp' (Just ds) _ -> return (Right (exp',ds))
     RewriteFailureM msg     -> return (Left msg)


