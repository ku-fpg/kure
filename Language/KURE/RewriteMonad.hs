-- |
-- Module: Language.KURE.RewriteMonad 
-- Copyright: (c) 2006-2008 Andy Gill
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- This is the definition of the monad inside KURE.

module Language.KURE.RewriteMonad 
  	( RewriteM	-- abstract
	, RewriteStatusM(..)
--	, RewriteF	-- special abstraction round RewriteM
	, runRewriteM
	, failM
	, catchM
	, chainM
	, liftQ
--	, focusM
	, idM
	, RewriteF(..)
--	, runRewriteF
	) where


import Control.Monad
import Data.Monoid
import Control.Applicative hiding (many)
import Data.Tree

------------------------------------------------------------------------------

data RewriteM m dec exp = 
   RewriteM { -- | 'runRewriteM' runs the 'RewriteM' monad, returning a status.
	      runRewriteM :: m (RewriteStatusM dec exp) 
	     }


data RewriteStatusM dec exp
     = RewriteWithDecM exp dec		-- ^ success implies that something changed in our rewrite world.
                                        -- 'dec' here are always new decs.
     | RewriteReturnM exp		-- ^ a regular success
     | RewriteFailureM String		-- ^ a real failure
--     | RewriteIdM exp			-- ^ identity marker on a value


-- TWO possible ways of thinking about rewriting:

-- C1 (e1) => C2 (C1 (e2)) => C3 (C2 (C1 (e3))) -- matches the *writer* like status
-- C1 (e1) => C1 (C2 (e2)) => C1 (C2 (C3 (e3))) -- will require mergeing??

instance (Monoid dec,Monad m) => Monad (RewriteM m dec) where
   return exp = RewriteM $ return $ RewriteReturnM exp
   (RewriteM m) >>= k = RewriteM $ do
   	     	      		 r <- m
				 case r of
				   RewriteWithDecM r ds -> continue r
					(\ e' ds' -> RewriteWithDecM e' (ds' `mappend` ds))
					(\ e'     -> RewriteWithDecM e' ds)
				   RewriteReturnM r -> continue r
					(\ e' ds' -> RewriteWithDecM e' ds')
					(\ e'     -> RewriteReturnM e')
				   RewriteFailureM msg -> return $ RewriteFailureM msg
--				   RewriteIdM r -> continue r
--					(\ e' ds' -> RewriteWithDecM e' ds')
--					(\ e'     -> RewriteReturnM e')
     where
	continue r mkDec mkRet = do
		r' <- runRewriteM (k r)
		return $ case r' of
			   RewriteWithDecM e' ds' -> mkDec e' ds'
			   RewriteReturnM e'      -> mkRet e'
			   RewriteFailureM msg    -> RewriteFailureM msg
--			   RewriteIdM e'	  -> mkRet e' 
				
   fail msg = RewriteM $ return $ RewriteFailureM msg

instance (Monoid dec,Monad m) => Functor (RewriteM m dec) where
  fmap f m = liftM f m

-- | 'liftQ' lets you tunnel into the inner monad, because 'RewriteM' is actually monad transformer.
liftQ :: (Monad m) =>  m a -> RewriteM m dec a   
liftQ m = RewriteM $          do r <- m
      	  	       	         return $ RewriteReturnM r

-- | 'failM' is our basic failure, with a String message.
failM :: (Monad m) => String -> RewriteM m dec a
failM msg = RewriteM $ return $ RewriteFailureM msg

-- | 'catchM' catches failures, and tries a second monadic computation.
catchM :: (Monad m) => RewriteM m dec a -> (String -> RewriteM m dec a) -> RewriteM m dec a
catchM (RewriteM m1) m2 = RewriteM $ do
	r <- m1 
	case r of
	  RewriteWithDecM _ _  -> return r
	  RewriteReturnM _     -> return r 
--	  RewriteIdM _         -> return r 
	  RewriteFailureM msg  -> runRewriteM (m2 msg)

-- 'RewriteF' is a function from some value to a monadic return value,
-- which can return the identity marker.
newtype RewriteF m dec e1 e2 = RewriteF { runRewriteF :: e1 -> RewriteM m dec (e2,Bool) }

-- | 'chainM' executes the first argument then the second, much like '>>=',
-- except that the second computation can see if the first computation was an identity or not.
-- Used to spot when a rewrite succeeded, but was the identity.

chainM :: (Monoid dec,Monad m) 
       => RewriteF m dec a b 
       -> (Bool -> Maybe dec -> RewriteF m dec b c)
       -> RewriteF m dec a c
chainM f1 k = RewriteF $ \ a -> RewriteM $ do
	r <- runRewriteM (runRewriteF f1 a)
	case r of
	  RewriteWithDecM (a,i) dec1 -> runRewriteM' i (runRewriteF (k i (Just dec1)) a)
	  RewriteReturnM (a,i)       -> runRewriteM' i (runRewriteF (k i Nothing) a)
	  RewriteFailureM msg        -> return $ RewriteFailureM msg -- and still fail 
  where
	runRewriteM' True m = runRewriteM m
	runRewriteM' False m = runRewriteM $ do 
					(r,_b) <- m
					return (r,False) 
					
-- no way for either to fail
{-
focusM :: (Monad m, Monoid dec) => (prod -> a) -> (prod -> a -> prod) 
       -> RewriteF m dec a a
       -> RewriteF m dec prod prod
focusM sel bld fn e = RewriteM $ do
	let a = sel e
	r <- runRewriteM (fn a)
	case r of
	  RewriteWithDecM a' dec1 -> return $ RewriteWithDecM (bld e a') dec1
	  RewriteReturnM a'       -> return $ RewriteReturnM (bld e a')
	  RewriteFailureM msg     -> return $ RewriteFailureM msg
	  RewriteIdM a'           -> return $ RewriteIdM e

-}

-- | The primitive id micro-rewrite.
idM :: (Monad m, Monoid dec) => RewriteF m dec a a
idM = RewriteF $ \ a -> RewriteM $ return $ RewriteReturnM (a,True)


   