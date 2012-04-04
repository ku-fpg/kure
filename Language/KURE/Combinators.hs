{-# LANGUAGE FlexibleInstances, UndecidableInstances, TypeFamilies #-}
-- |
-- Module: Language.KURE.Combinators
-- Copyright: (c) 2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- This module contains various combinators that use 'Translate' and 'Rewrite'. The convension is that
-- 'Translate' based combinators end with @T@, and 'Rewrite' based combinators end with @R@. Of course,
-- because 'Rewrite' is a type synonymm of 'Translate', the 'Rewrite' functions also operate with on 'Translate',
-- and the 'Translate' functions operate with 'Rewrite'.

module Language.KURE.Combinators
{-	(  -- * The 'Translate' combinators
	  (<+)
	, (>->)
	, failT
	, readerT
	, readEnvT
	, mapEnvT
	, writeEnvT
	, pureT
	, constT
	, concatT
	, -- * The 'Rewrite' combinators
	, (.+)
	, (!->)
	, tryR
	, changedR
	, repeatR
	, acceptR
	, idR
	, failR
	, -- * The Prelude combinators
	, tuple2R
	, listR
	, maybeR
	, tuple2U
	, listU
	, maybeU
	, -- * Generic failure, over both 'Monad's and 'Translate's.
	, Failable(..)
	, extractR
	, promoteR
	, extractU
    , promoteU
	, topdownR
	, bottomupR
	, alltdR
	, downupR
	, innermostR
	, foldU
    ) -} where

-- import Language.KURE.Types
import Types

import Data.Monoid
import Control.Applicative
import Control.Monad

infixl 3 <+, >->
-- infixr 3 ?

--------------------------------------------------------------------------------
-- The Translate combinators.

-- | like a catch, '<+' does the first 'Translate', and if it fails, then does the second 'Translate'.
(<+) :: MonadCatch m => Translate c m a b -> Translate c m a b -> Translate c m a b
t1 <+ t2 = translate $ \ ca -> apply t1 ca `catchM` (\ _ -> apply t2 ca)

-- | sequencing translates, the first must be a rewrite for the context to remain valid.
(>->) :: (Contextual c, Monad m) => Rewrite c m a -> Translate c m a b -> Translate c m a b
t1 >-> t2 = translate $ \ ca -> apply t1 ca >>= apply t2 . replaceC ca

-- | failing translation.
failT :: Monad m => String -> Translate c m a b
failT = fail

-- | look at the argument for the translation before choosing which 'Translate' to perform.
readerT :: (c a -> Translate c m a b) -> Translate c m a b
readerT f = translate $ \ ca -> apply (f ca) ca

-- | lift a function into a 'Translate'
liftT :: (Contextual c, Applicative m) => (a -> b) -> Translate c m a b
liftT f = translate (pure . f . extractC)

-- | 'constT' produces an unfailable 'Translate' that returns the first argument.
constT :: (Applicative m) => b -> Translate c m a b
constT b = translate (pure . const b)

-- | 'concatT' turns a list of 'Translate's that return a common 'Monoid'al result
-- into a single 'Translate' that performs them all in sequence and combines their
-- results with 'mconcat'
concatT :: (Monad m , Monoid b) => [Translate c m a b] -> Translate c m a b
concatT ts = translate (liftM mconcat . forM ts . flip apply)
 
-- | 'emptyT' is an unfailing 'Translate' that always returns 'mempty'
emptyT :: (Applicative m, Monoid b) => Translate c m a b
emptyT = constT mempty

--------------------------------------------------------------------------------

-- | identity rewrite.
idR :: (Contextual c, Applicative m) => Rewrite c m a
idR = rewrite (pure . extractC)

-- | catch a failing 'Rewrite', making it into an identity.
tryR :: (Contextual c, MonadCatch m) => Rewrite c m a -> Rewrite c m a
tryR s = s <+ idR

-- | repeat a rewrite until it fails, then return the result before the failure.
repeatR :: (Contextual c, MonadCatch m) => Rewrite c m a -> Rewrite c m a
repeatR s = tryR (s >-> repeatR s)

-- | look at the argument to a 'Rewrite', and choose to be either a failure or trivial success.
acceptR :: (Contextual c, Monad m) => (a -> Bool) -> Rewrite c m a
acceptR p = rewrite $ lowerC (\ a -> if p a then return a else fail "accept failed")
{-
--------------------------------------------------------------------------------
-- Prelude structures

tuple2R :: Rewrite a -> Rewrite b -> Rewrite (a,b)
tuple2R rra rrb = rewrite $ \ (a,b) -> liftM2 (,) (apply rra a) (apply rrb b)

listR :: Rewrite a -> Rewrite [a]
listR rr = rewrite $ mapM (apply rr)

maybeR :: Rewrite a -> Rewrite (Maybe a)
maybeR rr = rewrite $ \ e -> case e of
						Just e'  -> liftM Just (apply rr e')
						Nothing  -> return $ Nothing

tuple2U :: ( Monoid r) => Translate a r -> Translate b r -> Translate (a,b) r
tuple2U rra rrb = translate $ \ (a,b) -> liftM2 mappend (apply rra a) (apply rrb b)

listU :: ( Monoid r) => Translate a r -> Translate [a] r
listU rr = translate $ liftM mconcat . mapM (apply rr)

maybeU :: ( Monoid r) => Translate a r -> Translate (Maybe a) r
maybeU rr = translate $ \ e -> case e of
				Just e'  -> apply rr e'
				Nothing  -> return $ mempty

--------------------------------------------------------------------------------
-- | Guarded translate or monadic action.
(?) ::  Bool -> Translate a b -> Translate a b
(?) False _rr = failT "(False ?)"
(?) True   rr = rr

-- | 'extractR' converts a 'Rewrite' over a 'Generic' into a rewrite over a specific expression type.
extractR  :: (Term exp) => Rewrite  (Generic exp) -> Rewrite  exp	-- at *this* type
extractR rr = rewrite $ \ e -> do
            e' <- apply rr (inject e)
            case select e' of
                Nothing -> fail "extractR"
                Just r -> return r

-- | 'extractU' converts a 'Translate' taking a 'Generic' into a translate over a specific expression type.
extractU  :: (Term exp) => Translate  (Generic exp) r -> Translate  exp r
extractU rr = translate $ \ e -> apply rr (inject e)

-- | 'promoteR' promotes a 'Rewrite' into a 'Generic' 'Rewrite'; other types inside Generic cause failure.
-- 'try' can be used to convert a failure-by-default promotion into a 'id-by-default' promotion.
promoteR  :: (Term exp) => Rewrite  exp -> Rewrite  (Generic exp)
promoteR rr = rewrite $ \ e -> do
               case select e of
                 Nothing -> fail "promoteR"
                 Just e' -> do
                    r <- apply rr e'
                    return (inject r)

-- | 'promoteU' promotes a 'Translate' into a 'Generic' 'Translate'; other types inside Generic cause failure.
promoteU  :: (Term exp) => Translate  exp r -> Translate  (Generic exp) r
promoteU rr = translate $ \ e -> do
               case select e of
                 Nothing -> fail "promoteI"
                 Just e' -> apply rr e'

-------------------------------------------------------------------------------

-- | apply a rewrite in a top down manner.
topdownR :: ( e ~ Generic e, Term e) => Rewrite (Generic e) -> Rewrite (Generic e)
topdownR  s = s >-> allR (topdownR s)

-- | apply a rewrite in a bottom up manner.
bottomupR :: ( e ~ Generic e, Term e) => Rewrite (Generic e) -> Rewrite (Generic e)
bottomupR s = allR (bottomupR s) >-> s

-- | apply a rewrite in a top down manner, prunning at successful rewrites.
alltdR :: ( e ~ Generic e, Term e) => Rewrite (Generic e) -> Rewrite (Generic e)
alltdR    s = s <+ allR (alltdR s)

-- | apply a rewrite twice, in a topdown and bottom up way, using one single tree traversal.
downupR :: ( e ~ Generic e, Term e) => Rewrite (Generic e) -> Rewrite (Generic e)
downupR   s = s >-> allR (downupR s) >-> s

-- | a fixed point traveral, starting with the innermost term.
innermostR :: ( e ~ Generic e, Term e) => Rewrite (Generic e) -> Rewrite (Generic e)
innermostR s = bottomupR (tryR (s >-> innermostR s))

-- | repeatedly apply 'downupR s' until no further changes can be made
repeatedR :: ( e ~ Generic e, Term e) => Rewrite (Generic e) -> Rewrite (Generic e)
repeatedR s = downupR s !-> repeatedR s

-- fold a tree using a single translation for each node.
foldU :: ( e ~ Generic e, Term e, Monoid r) => Translate (Generic e) r -> Translate (Generic e) r
foldU s = concatT [ s, crushU (foldU s) ]


-}