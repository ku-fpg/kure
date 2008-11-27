{-# LANGUAGE ExistentialQuantification, TypeFamilies, Rank2Types, MultiParamTypeClasses, FlexibleContexts, GADTs #-}
-- |
-- Module: Language.KURE.RewriteMonad 
-- Copyright: (c) 2006-2008 Andy Gill
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- TODO: rename as RewriteM

module Language.KURE.Rewrite where
-- 	, Language.KURE.Translate.catch


import Control.Monad
import Data.Monoid
import Control.Applicative hiding (many)
import Data.Tree


-- infixr 3 >&>, >+>, >|>, >?>

------------------------------------------------------------------------------

type Path = [Int]

------------------------------------------------------------------------------

data RewriteM m dec exp = 
   RewriteM { runRewriteM :: m (RewriteStatusM dec exp) }

data RewriteStatusM dec exp
     = RewriteSuccessM exp dec
       		      	  		-- always at least 1 info,
                                        -- decs are only *new* decs
     | RewriteReturnM exp		-- unmarked success
     | RewriteFailureM String		-- a real failure

-- TWO possible ways of thinking about rewriting:

-- C1 (e1) => C2 (C1 (e2)) => C3 (C2 (C1 (e3))) -- matches the *writer* like status
-- C1 (e1) => C1 (C2 (e2)) => C1 (C2 (C3 (e3))) -- will require mergeing??

instance (Monoid dec,Monad m) => Monad (RewriteM m dec) where
   return exp = RewriteM $ return $ RewriteReturnM exp
{-
   (RewriteM m) >>= k = RewriteM $ \ path dec -> do
   	     	      		 r <- m path dec
				 case r of
				   RewriteSuccessM r ds -> do
				     r' <- runRewriteM (k r) path (ds `mappend` dec)
				     return $ 
				      case r' of
				       RewriteSuccessM e' ds'
				       		         -> RewriteSuccessM e' 
							    		   (ds' `mappend` ds)
				       RewriteReturnM e' -> RewriteSuccessM e' ds
				       RewriteFailureM msg -> RewriteFailureM msg
				   RewriteReturnM r -> do
				     r' <- runRewriteM (k r) path dec
				     return $
				      case r' of
				       RewriteSuccessM e' ds'
				       		         -> RewriteSuccessM e' 
									   ds'
				       RewriteReturnM e' -> RewriteReturnM e'
				       RewriteFailureM msg -> RewriteFailureM msg
				   RewriteFailureM msg -> return $ RewriteFailureM msg
   fail msg = RewriteM $ \ _ _ -> return $ RewriteFailureM msg
-}

liftQ :: (Monad m) =>  m a -> RewriteM m dec a   
liftQ m = RewriteM $          do r <- m
      	  	       	         return $ RewriteReturnM r

catch :: (Monad m) => RewriteM m dec a -> RewriteM m dec a -> RewriteM m dec a
catch (RewriteM m1) (RewriteM m2) = RewriteM $ do
	r <- m1 
	case r of
	  RewriteSuccessM _ _  -> return r
	  RewriteReturnM _     -> return r 
	  RewriteFailureM msg  -> m2
	
catchId :: (Monoid dec,Monad m) => RewriteM m dec a -> (Bool -> dec -> a -> RewriteM m dec a) -> RewriteM m dec a
catchId (RewriteM m1) k = RewriteM $ do
	r <- m1 
	case r of
	  RewriteSuccessM a dec1 -> runRewriteM (k False dec1 a)
	  RewriteReturnM a       -> runRewriteM (k True mempty a)
	  RewriteFailureM msg    -> return r -- and still fail 
{-
  where
      addDecs dec1 m = do
	 r' <- m
	 case r' of
	    RewriteSuccessM e dec2 -> return $ RewriteSuccessM e (dec1 `mappend` dec2)
	    RewriteReturnM e       -> return $ RewriteReturnM e
	    RewriteFailureM msg    -> return $ RewriteFailureM msg
-}

instance (Monoid dec,Monad m) => Functor (RewriteM m dec) where
  fmap f m = liftM f m
 
updateStatus 
	:: (Monad m, Monoid dec)
	=> (RewriteStatusM dec e -> RewriteStatusM dec e) 
	-> RewriteM m dec e
	-> RewriteM m dec e
updateStatus f m = RewriteM $ do
	r <- runRewriteM m 
	return (f r)

----------------------
{-
getPathM :: (Monad m) => RewriteM m dec Path
getPathM = RewriteM $ return $ RewriteReturnM path

addPathM :: (Monad m) =>  Int -> RewriteM m dec s -> RewriteM m dec s
addPathM ix (RewriteM m) = RewriteM $ m (path ++ [ix]) dec

----------------------

getDecsM :: (Monad m) => RewriteM m dec dec
getDecsM = RewriteM $ \ _ dec -> return $ RewriteReturnM dec

setDecsM :: (Monad m) => dec -> RewriteM m dec s -> RewriteM m dec s
setDecsM dec (RewriteM m) = RewriteM $ m path dec

updateDecsM :: (Monad m) => (dec -> dec) -> RewriteM m dec s -> RewriteM m dec s
updateDecsM f (RewriteM m) = RewriteM $ m path (f dec)
-}


