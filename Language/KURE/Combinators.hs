{-# LANGUAGE TypeFamilies #-}
-- |
-- Module: Language.KURE.Combinators
-- Copyright: (c) 2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- This module contains various combinators that use 'Translate' and 'Rewrite'. The convention is that
-- 'Translate' based combinators end with @T@, and 'Rewrite' based combinators end with @R@. Of course,
-- because 'Rewrite' is a type synonymm of 'Translate', the 'Rewrite' functions also operate with on 'Translate',
-- and the 'Translate' functions operate with 'Rewrite'.

module Language.KURE.Combinators
	(  -- * The 'Translate' combinators
	  (<+)
	, (>->)
	, failT
	, readerT
	, liftT
	, constT
	, concatT
        , emptyT
	  -- * The 'Rewrite' combinators
	, idR
	, tryR
	, repeatR
	, acceptR
	, (?)
	  -- * The 'Walker' combinators
	, extractR
	, promoteR
	, extractT
        , promoteT
	, topdownR
	, bottomupR
	, alltdR
	, downupR
	, innermostR
	, foldT
    )  where

import Language.KURE.Types

import Control.Monad
import Data.Pointed
import Data.Copointed
import Data.Monoid


infixl 3 <+, >->
infixr 3 ?

--------------------------------------------------------------------------------
-- The Translate combinators.

-- | like a catch, '<+' does the first 'Translate', and if it fails, then does the second 'Translate'.
(<+) :: MonadCatch m => Translate c m a b -> Translate c m a b -> Translate c m a b
t1 <+ t2 = translate $ \ ca -> apply t1 ca `catchM` (\ _ -> apply t2 ca)

-- | sequencing translates, the first must be a rewrite for the context to remain valid.
(>->) :: (EndoFunctor c, Monad m) => Rewrite c m a -> Translate c m a b -> Translate c m a b
t1 >-> t2 = translate $ \ ca -> apply t1 ca >>= apply t2 . replaceC ca

-- | failing translation.
failT :: Monad m => String -> Translate c m a b
failT = fail

-- | look at the argument for the translation before choosing which 'Translate' to perform.
readerT :: (c a -> Translate c m a b) -> Translate c m a b
readerT f = translate $ \ ca -> apply (f ca) ca

-- | lift a function into a 'Translate'
liftT :: (Copointed c, Pointed m) => (a -> b) -> Translate c m a b
liftT f = translate (point . f . copoint)

-- | 'constT' produces an unfailable 'Translate' that returns the first argument.
constT :: (Pointed m) => b -> Translate c m a b
constT b = translate (point . const b)

-- | 'concatT' turns a list of 'Translate's that return a common 'Monoid'al result
-- into a single 'Translate' that performs them all in sequence and combines their
-- results with 'mconcat'
concatT :: (Monad m , Monoid b) => [Translate c m a b] -> Translate c m a b
concatT ts = translate (liftM mconcat . forM ts . flip apply)

-- | 'emptyT' is an unfailing 'Translate' that always returns 'mempty'
emptyT :: (Pointed m, Monoid b) => Translate c m a b
emptyT = constT mempty

--------------------------------------------------------------------------------

-- | identity rewrite.
idR :: (Copointed c, Pointed m) => Rewrite c m a
idR = rewrite (point . copoint)

-- | catch a failing 'Rewrite', making it into an identity.
tryR :: (Copointed c, MonadCatch m) => Rewrite c m a -> Rewrite c m a
tryR s = s <+ idR

-- | repeat a rewrite until it fails, then return the result before the failure.
repeatR :: (EndoCopointed c, MonadCatch m) => Rewrite c m a -> Rewrite c m a
repeatR s = tryR (s >-> repeatR s)

-- | look at the argument to a 'Rewrite', and choose to be either a failure or trivial success.
acceptR :: (Copointed c, Monad m) => (a -> Bool) -> Rewrite c m a
acceptR p = rewrite $ lowerC (\ a -> if p a then return a else fail "accept failed")

-- | Guarded translate or monadic action.
(?) ::  Monad m => Bool -> Translate c m a b -> Translate c m a b
False ? _  = failT "(False ?)"
True  ? t  = t

--------------------------------------------------------------------------------

-- | 'extractT' converts a 'Translate' taking a 'Generic' into a translate over a specific expression type.
extractT :: (Walker c m a) => Translate c m (Generic a) b -> Translate c m a b
extractT t = translate (apply t . injectC)

-- | 'promoteT' promotes a 'Translate' into a 'Generic' 'Translate'; other types inside Generic cause failure.
promoteT  :: Walker c m a => Translate c m a b -> Translate c m (Generic a) b
promoteT t = translate (maybe (fail "promote") (apply t) . selectC)

-- | 'extractR' converts a 'Rewrite' over a 'Generic' into a rewrite over a specific expression type.
extractR :: Walker c m a => Rewrite c m (Generic a) -> Rewrite c m a
extractR r = extractT r >>= maybe (fail "extractR") return . select
  
-- | 'promoteR' promotes a 'Rewrite' into a 'Generic' 'Rewrite'; other types inside Generic cause failure.
--   'try' can be used to convert a failure-by-default promoteR into a 'id-by-default' promotion.
promoteR  :: Walker c m a => Rewrite c m a -> Rewrite c m (Generic a)
promoteR = liftM inject . promoteT

-------------------------------------------------------------------------------

-- | apply a 'Rewrite' in a top down manner.
topdownR :: (a ~ Generic a, Walker c m a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
topdownR r = r >-> allR (topdownR r)

-- | apply a 'Rewrite' in a bottom up manner.
bottomupR :: (a ~ Generic a, Walker c m a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
bottomupR r = allR (bottomupR r) >-> r

-- | apply a 'Rewrite' in a top down manner, prunning at successful rewrites.
alltdR :: (a ~ Generic a, Walker c m a, MonadCatch m) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
alltdR r = r <+ allR (alltdR r)

-- | apply a 'Rewrite' twice, in a topdown and bottom up way, using one single tree traversal.
downupR :: (a ~ Generic a, Walker c m a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
downupR r = r >-> allR (downupR r) >-> r

-- | a fixed point traveral, starting with the innermost term.
innermostR :: (a ~ Generic a, Walker c m a, Copointed c, MonadCatch m) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
innermostR r = bottomupR (tryR (r >-> innermostR r))

-- | fold a tree using a single 'Translate' for each node.
foldT :: (a ~ Generic a, Walker c m a, Monoid b) => Translate c m (Generic a) b -> Translate c m (Generic a) b
foldT t = concatT [ t, crushT (foldT t) ]

-------------------------------------------------------------------------------
