module Language.KURE.Combinators where
	
import Language.KURE.RewriteMonad as M	
import Language.KURE.Translate	
import Language.KURE.Rewrite	
import Data.Monoid


infixl 3 <+, >->, .+, !->

-- We use + for catching fail, * for catching id.

-- like catch, do the first, and if it fails, then do the second	
(<+) :: (Monoid dec, Monad m) => Translate m dec a b -> Translate m dec a b -> Translate m dec a b
(<+) rr1 rr2 = translateWith id $ \ dec e -> apply rr1 dec e `M.catchM` (\ _ -> apply rr2 dec e)

-- If the first one worked, then do to the second after the first.
(>->) :: (Monoid dec, Monad m) => Translate m dec a b -> Translate m dec b c -> Translate m dec a c
(>->) rr1 rr2 = translateWith id $ \ dec e -> do
	r <- apply rr1 dec e
        -- TODO: Key point, pull dec from result above, merge with dec below.
	apply rr2 dec r

tryR :: (Monoid dec, Monad m) => Rewrite m dec a -> Rewrite m dec a
tryR s = s <+ idR

repeatR :: (Monoid dec, Monad m) => Rewrite m dec a -> Rewrite m dec a
repeatR s = tryR (s >-> repeatR s) 

-- if the first rewrite is *id*, then do the second one.
(.+) :: (Monoid dec, Monad m) => Rewrite m dec a -> Rewrite m dec a -> Rewrite m dec a
(.+) a b = a `wasId` (\ i -> if i then b else idR)

-- if the first rewrite was *not* id, then also do the second.
(!->) :: (Monoid dec, Monad m) => Rewrite m dec a -> Rewrite m dec a -> Rewrite m dec a 
(!->) a b = a `wasId` (\ i -> if i then idR else b)

-- Turn id into a fail
unchangedR :: (Monoid dec,Monad m) => Rewrite m dec a -> Rewrite m dec a
unchangedR rr = rr .+ failT "unchanged"

reader :: (Monoid dec, Monad m) => (a -> Translate m dec a b) -> Translate m dec a b
reader fn = translateWith id $ \ dec expA -> apply (fn expA) dec expA

accept :: (Monoid dec, Monad m) => (a -> Bool) -> Rewrite m dec a
accept fn = translateWith id $ \ dec expA -> if fn expA 
				 	     then return expA
					    else fail "accept failed"
		
--------------------------------------------------------------------------------

-- internal to this module.
wasId :: (Monoid dec, Monad m) => Rewrite m dec a -> (Bool -> Rewrite m dec a) -> Rewrite m dec a
wasId rr fn = translateWith id $ \ dec e -> do
	-- TODO: check the order of mappend again.
	apply rr dec e `chainM` \ i dec' e' -> apply (fn i) (dec `mappend` dec') e'
					
