module Language.ER.Rewrite 
       ( Rewrite			-- syn
       , RewriteM			-- abstract
       , runRewrite
       , nullRewrite
       , liftQ
       , messageM
       , (>?>)				-- all these are right assoc
       , (>+>)
       , (>&>)
       , (>|>)
       , many
       , cut
       , cutM
       , getPathM
       , addPathM
       , addPath
       , info
       , Path
       , bindingsM
       , addBindingsM
       , liftBindingsM
       ) where

import Control.Monad
import Data.Monoid
import Data.Tree

import Debug.Trace


infixr 3 >&>, >+>, >|>, >?>

------------------------------------------------------------------------------

type Q a = IO a
-- import Language.Haskell.TH(Q)

type Path = [Int]

------------------------------------------------------------------------------

-- A rewrite takes an expression, and returns a transformed expression.
-- This is a type synonm, because we tighten the API higher up,
-- above any Subst engine.

type Rewrite info dec exp = exp -> RewriteM info dec exp 

-- A rewrite where we have already provided the input expression.

data RewriteM info dec exp = 
   RewriteM { runRewriteM_ :: Path -> [dec] -> Q (RewriteStatus info dec exp) }

-- Three modes
--   * success, with changes
--   * success, no changes (id)
--   * failure (exception, like pattern match failure)

data RewriteStatus info dec exp 
     = RewriteSuccess exp [Tree (Path,info)] [dec]	
       		      	  		-- always at least 1 info
     | RewriteId exp			-- nothing changed
     | RewriteFailure String		-- a real failure


-- The simple form, unability to rewrite in localize as an Null
-- exposing interface, perhaps the bindings listing will change

runRewrite :: Rewrite info dec exp 
	   -> Path 
	   -> [dec] 
	   -> exp 
	   -> Q (exp,[Tree (Path,info)],[dec])
runRewrite rewrite path decs exp = do
  res <- runRewriteM_ (rewrite exp) path decs
  case res of
     RewriteSuccess exp' is ds -> return (exp',is,ds)
     RewriteId exp             -> return (exp,[],[])
     RewriteFailure msg        -> fail msg


nullRewrite :: Rewrite info dec exp
nullRewrite =  return

instance Monad (RewriteM info dec) where
   return exp = RewriteM $ \ _path _dec -> return $ RewriteId exp
   (RewriteM m) >>= k = RewriteM $ \ path dec -> do
   	     	      		 r <- m path dec
				 case r of
				   RewriteSuccess r is ds -> do
				     r' <- runRewriteM_ (k r) path dec
				     return $ 
				      case r' of
				       RewriteSuccess e' is' ds'
				       		         -> RewriteSuccess e' 
							    		   (is ++ is') 
									   (ds ++ ds')
				       RewriteId e' -> RewriteSuccess e' is ds
				       RewriteFailure msg -> RewriteFailure msg
				   RewriteId r -> do
				     r' <- runRewriteM_ (k r) path dec
				     return $
				      case r' of
				       RewriteSuccess e' is' ds'
				       		         -> RewriteSuccess e' 
							    		   is' 
									   ds'
				       RewriteId e' -> RewriteId e'
				       RewriteFailure msg -> RewriteFailure msg
				   RewriteFailure msg -> return $ RewriteFailure msg
   fail msg = RewriteM $ \ _ _ -> return $ RewriteFailure msg

liftQ :: Q a -> RewriteM info dec a   
liftQ m = RewriteM $ \ _ _ -> do r <- m
      	  	       	         return $ RewriteId r

instance Functor (RewriteM info dec) where
  fmap f m = liftM f m

testSuccessM :: RewriteM info dec a -> RewriteM info dec (a,Bool)
testSuccessM (RewriteM m) = RewriteM $ \ path dec -> do
	    r <- m path dec
	    case r of
	       RewriteSuccess e' is ds -> return (RewriteSuccess (e',True) is ds)
	       RewriteId e'            -> return (RewriteId (e',False))
	       RewriteFailure msg      -> return (RewriteFailure msg)

-- small messages
messageM :: info -> RewriteM info dec ()
messageM info = RewriteM $ \ path dec -> return $ RewriteSuccess () [Node (path,info) []] []

(>?>) :: Rewrite info dec s  
      -> (Rewrite info dec s,Rewrite info dec s) 
      -> Rewrite info dec s
pred >?> (success,failure) = \ src -> do
     	 (inter,has_trans) <- testSuccessM (pred src)
	 if has_trans 
	    	 -- arguably, this should include the new bindings
	    then success inter
	    else failure inter

-- | do the first rewrite, and the second.

(>+>) :: Rewrite info dec s -> Rewrite info dec s -> Rewrite info dec s
before >+> after = before >?> (after,after)

-- | do the first rewrite, then second iff the first changed something.

(>&>) :: Rewrite info dec s -> Rewrite info dec s -> Rewrite info dec s
before >&> after = before >?> (after,nullRewrite)

-- | do the first rewrite, then second iff the first did not
--   changed anything.

(>|>) :: Rewrite info dec s -> Rewrite info dec s -> Rewrite info dec s
before >|> after = before >?> (nullRewrite,after)
     
many :: Rewrite info dec s -> Rewrite info dec s
many rewrite = rewrite >&> (many rewrite)

------------------------------------------------------------------------------

-- catches a failure, replacing it with the identity rewrite.

cut ::  Rewrite info dec s -> Rewrite info dec s
cut rr =  \ exp -> cutM exp (rr exp)

cutM :: s -> RewriteM info dec s -> RewriteM info dec s
cutM s (RewriteM m) = RewriteM $ \ path decs -> do
     r <- m path decs
     case r of
        RewriteSuccess e' is ds -> return (RewriteSuccess e' is ds)
	RewriteId e'            -> return (RewriteId e')
	RewriteFailure msg      -> return (RewriteId s)

getPathM = RewriteM $ \ path _ -> return $ RewriteSuccess path [] []

addPathM :: Int -> RewriteM info dec s -> RewriteM info dec s
addPathM ix (RewriteM m) = RewriteM $ \ path dec -> m (path ++ [ix]) dec

addPath :: Int -> Rewrite info dec s -> Rewrite info dec s
addPath ix rr =  \ exp -> addPathM ix (rr exp)

-- scoped messages
info :: (s -> s -> info) -> Rewrite info dec s -> Rewrite info dec s 
info i rr =  \ exp -> RewriteM $ \ path decs -> do 
       	       	         r <- runRewriteM_ (rr exp) path decs
			 case r of
			   RewriteSuccess exp' infos decs' -> return $ RewriteSuccess exp' [Node (path,i exp exp') infos] decs' 
			   RewriteId exp'                  -> return $ RewriteSuccess exp' [Node (path,i exp exp') []] []
			   RewriteFailure msg              -> return $ RewriteFailure msg


bindingsM :: RewriteM info dec [dec]
bindingsM = RewriteM $ \ path dec -> return $ RewriteId dec

addBindingsM :: [dec] -> RewriteM info dec a -> RewriteM info dec a
addBindingsM decs m = RewriteM $ \ path dec -> runRewriteM_ m path (dec ++ decs)

-- This dec *must* have a new name.
liftBindingsM :: [dec] -> RewriteM info dec ()
liftBindingsM decs = RewriteM $ \ path dec -> return $ RewriteSuccess () [] decs
