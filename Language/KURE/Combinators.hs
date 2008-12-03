module Language.KURE.Combinators where
	
import Language.KURE.Translate	
import Language.KURE.Rewrite as M	
import Data.Monoid


infixl 3 <+, >->, .+, !->

-- Should this return M ... (Bool)??
{-
testSuccessM :: (Monoid dec,Monad m) =>  RewriteM m dec a -> RewriteM m dec (a,Bool)
testSuccessM (RewriteM m) = RewriteM $ \ path dec -> do
	    r <- m path dec
	    case r of
	       RewriteSuccessM e' ds -> return (RewriteSuccessM (e',True) ds)
	       RewriteReturnM e'     -> return (RewriteReturnM (e',False))
	       RewriteFailureM msg   -> return (RewriteFailureM msg)
-}
	

-- ifM :: 
-- We use + for catching fail, * for catching id.

-- like catch, do the first, and if it fails, then do the second	
(<+) :: (Monoid dec, Monad m) => Translate m dec a b -> Translate m dec a b -> Translate m dec a b
(<+) rr1 rr2 = translateWith id $ \ dec e -> apply rr1 dec e `M.catch` apply rr2 dec e

-- If the first one worked, then do to the second after the first.
(>->) :: (Monoid dec, Monad m) => Translate m dec a b -> Translate m dec b c -> Translate m dec a c
(>->) rr1 rr2 = translateWith id $ \ dec e -> do
	r <- apply rr1 dec e
        -- TODO: Key point, pull dec from result above, merge with dec below.
	apply rr2 dec r

-- if the first rewrite is *id*, then do the second one.
(.+) :: (Monoid dec, Monad m) => Rewrite m dec a -> Rewrite m dec a -> Rewrite m dec a
(.+) a b = a `wasId` (\ i -> if i then b else idRewrite)

-- if the first rewrite was *not* id, then also do the second.
(!->) :: (Monoid dec, Monad m) => Rewrite m dec a -> Rewrite m dec a -> Rewrite m dec a 
(!->) a b = a `wasId` (\ i -> if i then idRewrite else b)

wasId :: (Monoid dec, Monad m) => Rewrite m dec a -> (Bool -> Rewrite m dec a) -> Rewrite m dec a
wasId rr fn = translateWith id $ \ dec e -> do
	-- TODO: check the order of mappend again.
	apply rr dec e `catchId` \ i dec' e' -> apply (fn i) (dec `mappend` dec') e'

-- Turn fail into an id
catchRewrite :: (Monoid dec,Monad m) => Rewrite m dec a -> Rewrite m dec a
catchRewrite rr = rr <+ idRewrite

-- Turn id into a fail
unchangedRewrite :: (Monoid dec,Monad m) => Rewrite m dec a -> Rewrite m dec a
unchangedRewrite rr = rr .+ failTranslate "unchanged"

reader :: (Monoid dec, Monad m) => (a -> Translate m dec a b) -> Translate m dec a b
reader fn = translateWith id $ \ dec expA -> apply (fn expA) dec expA

accept :: (Monoid dec, Monad m) => (a -> Bool) -> Rewrite m dec a
accept fn = translateWith id $ \ dec expA -> if fn expA 
				 	     then return expA
					    else fail "accept failed"
					
