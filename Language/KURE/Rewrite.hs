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
   RewriteM { runRewriteM :: Path -> dec -> m (RewriteStatusM dec exp) }

data RewriteStatusM dec exp
     = RewriteSuccessM exp dec
       		      	  		-- always at least 1 info,
                                        -- decs are only *new* decs
     | RewriteReturnM exp		-- unmarked success
     | RewriteFailureM String		-- a real failure

instance (Monoid dec,Monad m) => Monad (RewriteM m dec) where
   return exp = RewriteM $ \ _path _dec -> return $ RewriteReturnM exp
   (RewriteM m) >>= k = RewriteM $ \ path dec -> do
   	     	      		 r <- m path dec
				 case r of
				   RewriteSuccessM r ds -> do
				     r' <- runRewriteM (k r) path (ds `mappend` dec)
				     return $ 
				      case r' of
				       RewriteSuccessM e' ds'
				       		         -> RewriteSuccessM e' 
							    		   (ds `mappend` ds')
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

liftQ :: (Monad m) =>  m a -> RewriteM m dec a   
liftQ m = RewriteM $ \ _ _ -> do r <- m
      	  	       	         return $ RewriteReturnM r

catch :: (Monad m) => RewriteM m dec a -> RewriteM m dec a -> RewriteM m dec a
catch (RewriteM m1) (RewriteM m2) = RewriteM $ \ path dec -> do
	r <- m1 path dec
	case r of
	  RewriteSuccessM _ _  -> return r
	  RewriteReturnM _     -> return r 
	  RewriteFailureM msg  -> m2 path dec
	
catchId :: (Monoid dec,Monad m) => RewriteM m dec a -> (Bool -> a -> RewriteM m dec a) -> RewriteM m dec a
catchId (RewriteM m1) k = RewriteM $ \ path dec -> do
	r <- m1 path dec
	case r of
	  RewriteSuccessM a dec1 -> addDecs dec1 $ runRewriteM (k False a) path dec
	  RewriteReturnM a     -> runRewriteM (k True a) path dec
	  RewriteFailureM msg  -> return r -- and still fail 
  where
      addDecs dec1 m = do
	 r' <- m
	 case r' of
	    RewriteSuccessM e dec2 -> return $ RewriteSuccessM e (dec1 `mappend` dec2)
	    RewriteReturnM e       -> return $ RewriteReturnM e
	    RewriteFailureM msg    -> return $ RewriteFailureM msg

instance (Monoid dec,Monad m) => Functor (RewriteM m dec) where
  fmap f m = liftM f m
 
updateStatus 
	:: (Monad m, Monoid dec)
	=> (RewriteStatusM dec e -> RewriteStatusM dec e) 
	-> RewriteM m dec e
	-> RewriteM m dec e
updateStatus f m = RewriteM $ \ path dec -> do
	r <- runRewriteM m path dec
	return (f r)

----------------------

getPathM :: (Monad m) => RewriteM m dec Path
getPathM = RewriteM $ \ path _ -> return $ RewriteReturnM path

addPathM :: (Monad m) =>  Int -> RewriteM m dec s -> RewriteM m dec s
addPathM ix (RewriteM m) = RewriteM $ \ path dec -> m (path ++ [ix]) dec
