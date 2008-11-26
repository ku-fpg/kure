module Language.KURE.Combinators where
	
import Language.KURE.Translate	
import Language.KURE.Rewrite as M	

infixl 3 <+, >->, .+, !->

-- Should this return M ... (Bool)??
{-
testSuccessM :: (Decs dec,Monad m) =>  RewriteM m dec a -> RewriteM m dec (a,Bool)
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
(<+) :: (Decs dec, Monad m) => Translate m dec a b -> Translate m dec a b -> Translate m dec a b
(<+) rr1 rr2 = translateWith id $ \ e -> apply rr1 e `M.catch` apply rr2 e

-- If the first one worked, then do to the second after the first.
(>->) :: (Decs dec, Monad m) => Translate m dec a b -> Translate m dec b c -> Translate m dec a c
(>->) rr1 rr2 = translateWith id $ \ e -> do
	r <- apply rr1 e
	apply rr2 r

-- if the first rewrite is *id*, then do the second one.
(.+) :: (Decs dec, Monad m) => Rewrite m dec a -> Rewrite m dec a -> Rewrite m dec a
(.+) a b = a `wasId` (\ i -> if i then b else idRewrite)

-- if the first rewrite was *not* id, then also do the second.
(!->) :: (Decs dec, Monad m) => Rewrite m dec a -> Rewrite m dec a -> Rewrite m dec a 
(!->) a b = a `wasId` (\ i -> if i then idRewrite else b)

wasId :: (Decs dec, Monad m) => Rewrite m dec a -> (Bool -> Rewrite m dec a) -> Rewrite m dec a
wasId rr fn = translateWith id $ \ e -> do
	apply rr e `catchId` \ i e' -> apply (fn i) e'

-- Turn fail into an id
catchRewrite :: (Decs dec,Monad m) => Rewrite m dec a -> Rewrite m dec a
catchRewrite rr = rr <+ idRewrite

-- Turn id into a fail
unchangedRewrite :: (Decs dec,Monad m) => Rewrite m dec a -> Rewrite m dec a
unchangedRewrite rr = rr .+ failTranslate "unchanged"

reader :: (Decs dec, Monad m) => (a -> Translate m dec a b) -> Translate m dec a b
reader fn = translateWith id $ \ expA -> apply (fn expA) expA

accept :: (Decs dec, Monad m) => (a -> Bool) -> Rewrite m dec a
accept fn = translateWith id $ \ expA -> if fn expA 
					 then return expA
					 else fail "accept failed"
					
