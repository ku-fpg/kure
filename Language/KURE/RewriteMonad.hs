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
	, updateStatus
	, runRewriteM
	, failM
	, catchM
	, chainM
	, liftQ
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
     = RewriteSuccessM exp dec		-- ^ success implies that something changed in our rewrite world.
                                        -- 'dec' here are always new decs.
     | RewriteReturnM exp		-- ^ a regular success
     | RewriteFailureM String		-- ^ a real failure

-- TWO possible ways of thinking about rewriting:

-- C1 (e1) => C2 (C1 (e2)) => C3 (C2 (C1 (e3))) -- matches the *writer* like status
-- C1 (e1) => C1 (C2 (e2)) => C1 (C2 (C3 (e3))) -- will require mergeing??

instance (Monoid dec,Monad m) => Monad (RewriteM m dec) where
   return exp = RewriteM $ return $ RewriteReturnM exp
   (RewriteM m) >>= k = RewriteM $ do
   	     	      		 r <- m
				 case r of
				   RewriteSuccessM r ds -> do
				     r' <- runRewriteM (k r)
				     return $ 
				      case r' of
				       RewriteSuccessM e' ds'
				       		         -> RewriteSuccessM e' 
							    		   (ds' `mappend` ds) -- TODO: choose!
				       RewriteReturnM e' -> RewriteSuccessM e' ds
				       RewriteFailureM msg -> RewriteFailureM msg
				   RewriteReturnM r -> do
				     r' <- runRewriteM (k r)
				     return $
				      case r' of
				       RewriteSuccessM e' ds'
				       		         -> RewriteSuccessM e' 
									   ds'
				       RewriteReturnM e' -> RewriteReturnM e'
				       RewriteFailureM msg -> RewriteFailureM msg
				   RewriteFailureM msg -> return $ RewriteFailureM msg
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

-- | 'catchM' catches failures, and trys a second monadic computation.
catchM :: (Monad m) => RewriteM m dec a -> (String -> RewriteM m dec a) -> RewriteM m dec a
catchM (RewriteM m1) m2 = RewriteM $ do
	r <- m1 
	case r of
	  RewriteSuccessM _ _  -> return r
	  RewriteReturnM _     -> return r 
	  RewriteFailureM msg  -> runRewriteM (m2 msg)
	
-- | 'chainM' executes the first argument then the second, much like '>>=',
-- except that the second computation can see if the first computation was an identity or not.
-- Used to spot when a rewrite succeeded, but was uneffective.

chainM :: (Monoid dec,Monad m) => RewriteM m dec a -> (Bool -> dec -> a -> RewriteM m dec b) -> RewriteM m dec b
chainM (RewriteM m1) k = RewriteM $ do
	r <- m1 
	case r of
	  RewriteSuccessM a dec1 -> runRewriteM (k False dec1 a)
	  RewriteReturnM a       -> runRewriteM (k True mempty a)
	  RewriteFailureM msg    -> return $ RewriteFailureM msg -- and still fail 
 
-- | 'updateStatus' updates the result status of the argument's computation.
updateStatus 
	:: (Monad m, Monoid dec)
	=> (RewriteStatusM dec e -> RewriteStatusM dec' e) 
	-> RewriteM m dec e
	-> RewriteM m dec' e
updateStatus f m = RewriteM $ do
	r <- runRewriteM m 
	return (f r)

