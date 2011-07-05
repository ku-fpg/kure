{-# LANGUAGE FlexibleInstances, UndecidableInstances, TypeFamilies #-}
-- |
-- Module: Language.KURE.Combinators
-- Copyright: (c) 2010 The University of Kansas
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- This module contains various combinators that use 'Translate' and 'Rewrite'. The convension is that
-- 'Translate' based combinators end with @T@, and 'Rewrite' based combinators end with @R@. Of course,
-- because 'Rewrite' is a type synomim of 'Translate', the 'Rewrite' functions also operate with on 'Translate',
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

import Language.KURE.Types

import Data.Monoid
import Control.Arrow
import qualified Control.Category as Cat
import Control.Monad

--infixl 3 <+, >->, .+, !->
infixr 3 ?

-- Note: We use < for catching fail, . for catching id.

--------------------------------------------------------------------------------
-- The Translate combinators.

-- | like a catch, '<+' does the first translate, and if it fails, then does the second translate.
(<+) :: Translate a b -> Translate a b -> Translate a b
(<+) rr1 rr2 = translate $ \ e -> apply rr1 e `catchTM` (\ _ -> apply rr2 e)

(>->) :: Translate a b -> Translate b c -> Translate a c
(>->) rr1 rr2 = rr1 >>> rr2

-- | failing translation.
failT :: String -> Translate a b
failT msg = translate $ \ _ -> fail msg

-- | look at the argument for the translation before choosing which translation to perform.
readerT :: (a -> Translate a b) -> Translate a b
readerT fn = translate $ \ expA -> apply (fn expA) expA

-- | lift a function into a Translate
pureT :: (a -> b) -> Translate a b
pureT = arr

-- | 'constT' always translates into an unfailable 'Translate' that returns the first argument.
constT :: b -> Translate a b
constT = pureT . const

-- | 'concatT' turns a list of 'Translate's that return a common 'Monoid'al result
-- into a single 'Translate' that performs them all in sequence and combines their
-- results with 'mconcat'
concatT :: (Monoid r) => [Translate a r] -> Translate a r
concatT ts = translate $ \ e -> do
	rs <- sequence [ apply t e | t <- ts ]
	return (mconcat rs)

-- | 'emptyT' is an unfailing 'Translate' that always returns 'mempty'
emptyT :: (Monoid r) => Translate a r
emptyT = constT mempty

--------------------------------------------------------------------------------
-- The 'Rewrite' combinators.
-- | if the first rewrite is an identity, then do the second rewrite.
(.+) :: (Term a) => Rewrite a -> Rewrite a -> Rewrite a
(.+) r0 r1 = rewrite $ \ e0 -> do
		e1 <- apply r0 e0
		isId <- e0 .==. e1
		if isId then apply r1 e1
			    else return e1

-- | if the first rewrite was /not/ an identity, then also do the second rewrite.
(!->) :: (Term a) => Rewrite a -> Rewrite a -> Rewrite a
(!->) r0 r1 = rewrite $ \ e0 -> do
		e1 <- apply r0 e0
		isId <- e0 .==. e1
		if isId then return e1
			    else apply r1 e1

-- | Term equality
(.==.) :: (TranslateMonad m, Term e) => e -> e -> m Bool
(.==.) = apply . equals

-- | catch a failing 'Rewrite', making it into an identity.
tryR :: Rewrite a -> Rewrite a
tryR s = s <+ idR

-- | if this is an identity rewrite, make it fail. To succeed, something must have changed.
changedR :: (Term a) => Rewrite a -> Rewrite a
changedR rr = rr .+ failR "unchanged"

-- | repeat a rewrite until it fails, then return the result before the failure.
repeatR :: Rewrite a -> Rewrite a
repeatR s = tryR (s >-> repeatR s)

-- | look at the argument to a rewrite, and choose to be either a failure of trivial success.
acceptR :: (a -> Bool) -> Rewrite a
acceptR fn = translate $ \  expA -> if fn expA
                				    then return expA
			                	    else fail "accept failed"

-- | identity rewrite.
idR :: Rewrite exp
idR = Cat.id

-- | failing rewrite.
failR :: String -> Rewrite a
failR = failT

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

-- | repeated apply 'downupR s' until no further changes can be made
repeatedR :: ( e ~ Generic e, Term e) => Rewrite (Generic e) -> Rewrite (Generic e)
repeatedR s = downupR s !-> repeatedR s

-- fold a tree using a single translation for each node.
foldU :: ( e ~ Generic e, Term e, Monoid r) => Translate (Generic e) r -> Translate (Generic e) r
foldU s = concatT [ s, crushU (foldU s) ]


