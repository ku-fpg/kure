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
import Control.Category
import Control.Arrow
import Data.Monoid
import Control.Applicative hiding (many)
import Data.Tree

import Language.KURE.RewriteMonad

-- | 'Translate' is a translation or strategy that translates between @exp1@ and @exp2@, with the posiblity of failure,
-- and remembers identity translations.

newtype Translate m dec exp1 exp2 =
    Translate { applyTranslate :: dec -> exp1 -> RewriteM m dec exp2}

-- | 'apply' directly applies a 'Translate' value to an argument.
apply :: Translate m dec exp1 exp2 -> dec -> exp1 -> RewriteM m dec exp2
apply = applyTranslate

-- | 'translate' is the standard way of building a 'Translate', where if the translation is successful it 
-- is automatically marked as a non-identity translation. 
--
-- Note: @translate $ \ _ e -> return e@ /is not/ an identity rewrite, but a succesfull rewrite that
-- returns its provided argument.

translate :: (Monoid dec, Monad m) => (dec -> exp1 -> RewriteM m dec exp2) -> Translate m dec exp1 exp2
translate = Translate


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
     RewriteWithDecM exp' ds -> return (Right (exp',ds))
     RewriteReturnM exp'     -> return (Right (exp',mempty))
     RewriteIdM     exp'     -> return (Right (exp',mempty))
     RewriteFailureM msg     -> return (Left msg)

{-
instance (Monad m, Monoid dec) => Category (Translate m dec) where
  id = translate $ \ _ e -> return e
  (.) rr2 rr1 = 
	translate $ \ dec e ->
	apply rr1 dec e `chainM` \ _i dec' e' -> apply rr2 (dec `mappend` dec') e'

instance (Monad m, Monoid dec) => Arrow (Translate m dec) where
  arr f = translate $ \ env a -> return (f a)	-- non-id operation
  first tr = translate $ \ env (b,d) -> do c <- apply tr env b
 					   return (c,d)
instance (Monad m, Monoid dec) => ArrowZero (Translate m dec) where
  arrowZero f = translate $ \ env a -> return (f a)	-- non-id operation
  first tr = translate $ \ env (b,d) -> do c <- apply tr env b
 					   return (c,d)

-}



