{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, ExistentialQuantification, Rank2Types #-}

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
	, translate
	) where

import Control.Category 
import Control.Arrow
import Language.KURE.RewriteMonad

-- | 'Translate' is a translation or strategy that translates between @exp1@ and @exp2@, with the posiblity of failure,
-- and remembers identity translations.

data Translate exp1 exp2 = Translate (forall m . (TranslateMonad m) => exp1 -> m exp2 )

-- | 'apply' directly applies a 'Translate' value to an argument.
apply :: (TranslateMonad m) => Translate exp1 exp2 -> exp1 -> m exp2
apply (Translate t) exp1 = t exp1 

-- | 'translate' is the standard way of building a 'Translate', where if the translation is successful it 
-- is automatically marked as a non-identity translation. 

translate :: (forall m . (TranslateMonad m) => exp1 -> m exp2) -> Translate exp1 exp2
translate f = Translate $ \ e -> f e

instance Category (Translate) where
   id = translate $ return 
   (.) rr1 rr2 = translate $ \ e -> apply rr2 e >>= apply rr1

instance Arrow (Translate) where
   arr f = translate (return Prelude.. f)
   first rr = translate $ \ (b,d) -> do c <- apply rr b ; return (c,d)

{-
-- | 'runTranslate' executes the translation, returning either a failure message,
-- or a success and the new parts of the environment.

runTranslate :: (Monoid dec,Monad m) 
	   => Translate m dec exp res
	   -> exp 
	   -> m (Either String (res,dec,Int))
runTranslate rr dec e = do
  res <- runRewriteM (apply rr e) dec
  case res of
     RewriteReturnM exp' Nothing c -> return (Right (exp',mempty,theCount c))
     RewriteReturnM exp' (Just ds) c -> return (Right (exp',ds,theCount c))
     RewriteFailureM msg     -> return (Left msg)
-}
