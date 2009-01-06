-- |
-- Module: Language.KURE.Combinators 
-- Copyright: (c) 2006-2008 Andy Gill
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
	(  -- * The 'Translate' combinators
	  (<+)
	, (>->)
	, failT
	, readerT
	, getDecsT
	, mapDecsT
	, pureT
	, constT
	, concatT
	, -- * The 'Rewrite' combinators
	  (.+)
	, (!->)
	, tryR
	, changedR
	, repeatR
	, acceptR
	, idR
	, failR
	, -- * The Prelude combinators
	  tuple2R
	, listR
	, maybeR
	, tuple2U
	, listU
	, maybeU
	, -- * Generic failure, over both 'Monad's and 'Translate's.
	  (?)
	, Failable(..)
	) where 
	
import Language.KURE.RewriteMonad	
import Language.KURE.Translate	
import Language.KURE.Rewrite	
import Data.Monoid
import Control.Monad

infixl 3 <+, >->, .+, !->
infixr 3 ?

-- Note: We use < for catching fail, . for catching id.

--------------------------------------------------------------------------------
-- The Translate combinators.

-- | like a catch, '<+' does the first translate , and if it fails, then does the second translate.	
(<+) :: (Monoid dec, Monad m) => Translate m dec a b -> Translate m dec a b -> Translate m dec a b
(<+) rr1 rr2 = translate $ \ e -> transparently $ apply rr1 e `catchM` (\ _ -> apply rr2 e)

-- | like a @;@ If the first translate succeeds, then do to the second translate after the first translate.
(>->) :: (Monoid dec, Monad m) => Translate m dec a b -> Translate m dec b c -> Translate m dec a c
(>->) rr1 rr2 = translate $ \ e -> transparently $ chainM (apply rr1 e) ( \ _i e2 -> apply rr2 e2)

-- | failing translation.
failT :: (Monad m, Monoid dec) => String -> Translate m dec a b
failT msg = translate $ \ _ -> failM msg


-- | look at the argument for the translation before choosing which translation to perform. 
readerT :: (Monoid dec, Monad m) => (a -> Translate m dec a b) -> Translate m dec a b
readerT fn = translate $ \ expA -> transparently $ apply (fn expA) expA

-- | look at the @dec@ before choosing which translation to do.
getDecsT :: (Monad m, Monoid dec) => (dec -> Translate m dec a b) -> Translate m dec a b
getDecsT f = translate $ \ e -> transparently $
                                do dec <- getDecsM 
                                   apply (f dec) e

-- | change the @dec@'s for a scoped translation.
mapDecsT :: (Monoid dec,Monad m) => (dec -> dec) -> Translate m dec a r -> Translate m dec a r
mapDecsT f_env rr = translate $ \ e -> mapDecsM f_env (apply rr e)

-- | 'pureT' promotes a function into an unfailable, non-identity 'Translate'.
pureT :: (Monad m,Monoid dec) => (a -> b) -> Translate m dec a b
pureT f = translate $ \ a -> return (f a)

-- | 'constT' always translates into an unfailable 'Translate' that returns the first argument.
constT :: (Monad m,Monoid dec) => b -> Translate m dec a b
constT = pureT . const

-- | 'concatT' composes a list of 'Translate' into a single 'Translate' which 'mconcat's its result.
concatT :: (Monad m,Monoid dec,Monoid r) => [Translate m dec a r] -> Translate m dec a r
concatT ts = translate $ \ e -> do
	rs <- sequence [ apply t e | t <- ts ]
	return (mconcat rs)
--------------------------------------------------------------------------------
-- The 'Rewrite' combinators.

-- | if the first rewrite is an identity, then do the second rewrite.
(.+) :: (Monoid dec, Monad m) => Rewrite m dec a -> Rewrite m dec a -> Rewrite m dec a
(.+) a b = a `wasId` (\ i -> if i then b else idR)

-- | if the first rewrite was /not/ an identity, then also do the second rewrite.
(!->) :: (Monoid dec, Monad m) => Rewrite m dec a -> Rewrite m dec a -> Rewrite m dec a 
(!->) a b = a `wasId` (\ i -> if i then idR else b)

-- | catch a failing 'Rewrite', making it into an identity.
tryR :: (Monoid dec, Monad m) => Rewrite m dec a -> Rewrite m dec a
tryR s = s <+ idR

-- | if this is an identity rewrite, make it fail. To succeed, something must have changed.
changedR :: (Monoid dec,Monad m) => Rewrite m dec a -> Rewrite m dec a
changedR rr = rr .+ failR "unchanged"

-- | repeat a rewrite until it fails, then return the result before the failure.
repeatR :: (Monoid dec, Monad m) => Rewrite m dec a -> Rewrite m dec a
repeatR s = tryR (s >-> repeatR s) 

-- | look at the argument to a rewrite, and choose to be either a failure of trivial success.
acceptR :: (Monoid dec, Monad m) => (a -> Bool) -> Rewrite m dec a
acceptR fn = translate $ \  expA -> transparently $
                                    if fn expA 
				    then return expA
				    else fail "accept failed"


-- | identity rewrite.
idR :: (Monad m, Monoid dec) => Rewrite m dec exp
idR = rewrite $ \ e -> transparently $ return e

-- | failing rewrite.
failR :: (Monad m, Monoid dec) => String -> Rewrite m dec a
failR = failT

--------------------------------------------------------------------------------
-- Prelude structures

tuple2R :: (Monoid dec, Monad m) => Rewrite m dec a -> Rewrite m dec b -> Rewrite m dec (a,b)
tuple2R rra rrb = rewrite $ \ (a,b) -> transparently $ liftM2 (,) (apply rra a) (apply rrb b)

listR :: (Monoid dec, Monad m) => Rewrite m dec a -> Rewrite m dec [a]
listR rr = rewrite $ transparently . mapM (apply rr)

maybeR :: (Monoid dec, Monad m) => Rewrite m dec a -> Rewrite m dec (Maybe a)
maybeR rr = rewrite $ \ e -> transparently $ case e of
						Just e'  -> liftM Just (apply rr e')
						Nothing  -> return $ Nothing

tuple2U :: (Monoid dec, Monad m, Monoid r) => Translate m dec a r -> Translate m dec b r -> Translate m dec (a,b) r
tuple2U rra rrb = translate $ \ (a,b) -> liftM2 mappend (apply rra a) (apply rrb b)

listU :: (Monoid dec, Monad m, Monoid r) => Translate m dec a r -> Translate m dec [a] r
listU rr = translate $ liftM mconcat . mapM (apply rr)

maybeU :: (Monoid dec, Monad m, Monoid r) => Translate m dec a r -> Translate m dec (Maybe a) r
maybeU rr = translate $ \ e -> case e of
				Just e'  -> apply rr e'
				Nothing  -> return $ mempty

-- | Failable structure.
class Failable f where
  failure :: String -> f a

instance (Monad m, Monoid dec) => Failable (Translate m dec a) where 
  failure msg = failT msg

instance (Monad m, Monoid dec) => Failable (RewriteM m dec) where 
  failure msg = fail msg
 
-- | Guarded translate or monadic action.
(?) ::  (Failable f) => Bool -> f a -> f a
(?) False _rr = failure "(False ?)"
(?) True   rr = rr


--------------------------------------------------------------------------------
-- internal to this module.
wasId :: (Monoid dec, Monad m) => Rewrite m dec a -> (Bool -> Rewrite m dec a) -> Rewrite m dec a
wasId rr fn = translate $ \ e -> transparently $
	chainM (apply rr e)
	       (\ i e' -> apply (fn i) e')

