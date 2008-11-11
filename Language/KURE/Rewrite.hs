{-# LANGUAGE ExistentialQuantification, TypeFamilies, Rank2Types, MultiParamTypeClasses, FlexibleContexts, GADTs #-}
-- |
-- Module: Language.KURE.Rewrite 
-- Copyright: (c) 2006-2008 Andy Gill
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc
--

module Language.KURE.Rewrite 
       ( Rewrite			-- syn
       , RewriteM			-- abstract
       , runRewrite
       , nullRewrite
       , liftQ
       , (>?>)				-- all these are right assoc
       , (>+>)
       , (>&>)
       , (>|>)
       , many
       , cut
       , cutM
       , getPathM
       , addPathM
--       , addPath
       , Path
       , bindingsM
       , addBindingsM
       , liftBindingsM
--       , Node(..) 
 --      , Subst(..)
       ) where

import Control.Monad
import Data.Monoid
import Control.Applicative hiding (many)
import Data.Tree

infixr 3 >&>, >+>, >|>, >?>

------------------------------------------------------------------------------

type Path = [Int]

------------------------------------------------------------------------------

-- A rewrite takes an expression, and returns a transformed expression.
-- This is a type synonm, because we tighten the API higher up,
-- above any Subst engine.

-- | A rewrite can either
--    * succeed (something changed)
--    * fail
--    * changeless (nothing failed, nothing changed)
-- This intentionally an exported synonim.

type Rewrite m dec exp = Translate m dec exp exp

-- | A strategy can either
--    * succeed (something changed)
--    * fail
--  A endomorphic translate may also be changeless.

newtype Translate m dec exp1 exp2 =
    Rewrite { unRewrite :: exp1 -> Path -> dec -> m (RewriteStatus dec exp1 exp2) }

data RewriteM m dec exp = 
   RewriteM { runRewriteM :: Path -> dec -> m (RewriteStatusM dec exp) }

-- Three modes
--   * success, with changes, and perhaps new scoping bindings.
--   * success, with no location (yet)
--   * failure (exception, like pattern match failure)

data RewriteStatus dec exp1 exp2 where
 	-- always at least 1 info,
        -- decs are only *new* decs
    RewriteSuccess :: exp2 -> dec -> RewriteStatus dec exp1 exp2
	-- a real failure
    RewriteFailure :: String -> RewriteStatus dec exp1 exp2
	-- nothing changed
    RewriteChangeless :: (exp1 ~ exp2) => RewriteStatus dec exp1 exp2

data RewriteStatusM dec exp
     = RewriteSuccessM exp dec
       		      	  		-- always at least 1 info,
                                        -- decs are only *new* decs
     | RewriteReturnM exp		-- unmarked success
     | RewriteFailureM String		-- a real failure

-- This needs to be called out as a function to allow the GADT's to typecheck.
statusToStatusM :: exp1 -> RewriteStatus dec exp1 exp2 -> RewriteStatusM dec exp2
statusToStatusM exp1 (RewriteSuccess exp dec) = RewriteSuccessM exp dec
statusToStatusM exp1 (RewriteFailure msg)          = RewriteFailureM msg
statusToStatusM exp1 (RewriteChangeless)           = RewriteReturnM exp1

apply :: (Monad m) => Translate m dec exp1 exp2 -> exp1 -> RewriteM m dec exp2
apply (Rewrite action) exp = RewriteM $ \ path decs -> do
  r <- action exp path decs
  return (statusToStatusM exp r)

-- This promotes a monadic action into a Rewrite. Any non-failure is turned into
-- succeess, with a location tag.

-- so: rewrite $ \ (Add (Val x) (Val y)) -> return (Var (x + y))
-- is marked as a rewritten

rewrite :: (Monad m,Monoid dec) => (exp1 -> RewriteM m dec exp2) -> Translate m dec exp1 exp2
rewrite action = Rewrite $ \ exp path dec -> do
  r <- runRewriteM (action exp) path dec
  case r of
    RewriteSuccessM exp dec -> return $ RewriteSuccess exp dec
    RewriteFailureM msg		 -> return $ RewriteFailure msg
    RewriteReturnM exp	         -> return $ RewriteSuccess exp mempty

-- precondition, if RewriteM returns, it returns with the same value as given.
-- one way of knowning this is if the only things that could change the structure
-- are sub-calls to RewriteM.

-- rebuild??

rewriteWithId :: (Monad m,Monoid dec) => (exp -> RewriteM m dec exp) -> Rewrite m dec exp
rewriteWithId action = Rewrite $ \ exp path dec -> do
  r <- runRewriteM (action exp) path dec
  case r of
    RewriteSuccessM exp dec -> return $ RewriteSuccess exp dec
    RewriteFailureM msg		 -> return $ RewriteFailure msg
    RewriteReturnM _exp	         -> return $ RewriteChangeless -- assert: _exp == exp, *type* enforced by GADTs

-- The simple form, unability to rewrite in localize as an Null
-- exposing interface, perhaps the bindings listing will change

runRewrite :: (Decs dec,Monad m) =>  Rewrite m dec exp 
	   -> Path 
	   -> dec 
	   -> exp 
	   -> m (exp,dec)
runRewrite rr path decs exp = do
  res <- runRewriteM (apply rr exp) path decs
  case res of
     RewriteSuccessM exp' ds -> return (exp',ds)
     RewriteReturnM exp             -> return (exp,mempty)
     RewriteFailureM msg        -> fail msg


nullRewrite :: (Decs dec,Monad m) =>  Rewrite m dec exp
nullRewrite =  Rewrite $ \ _ _ _ -> return (RewriteChangeless)

instance (Decs dec,Monad m) => Monad (RewriteM m dec) where
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

instance (Decs dec,Monad m) => Functor (RewriteM m dec) where
  fmap f m = liftM f m

------------------------------------------------------------------------------

testSuccessM :: (Decs dec,Monad m) =>  RewriteM m dec a -> RewriteM m dec (a,Bool)
testSuccessM (RewriteM m) = RewriteM $ \ path dec -> do
	    r <- m path dec
	    case r of
	       RewriteSuccessM e' ds -> return (RewriteSuccessM (e',True) ds)
	       RewriteReturnM e'            -> return (RewriteReturnM (e',False))
	       RewriteFailureM msg      -> return (RewriteFailureM msg)

(>?>) :: (Decs dec,Monad m) 
      =>  Translate m dec s1 s2
      -> (Translate m dec s2 s3,Translate m dec s2 s3)
      -> Translate m dec s1 s3
pred >?> (success,failure) = rewrite $ \ src -> do
     	 (inter,has_trans) <- testSuccessM (apply pred src)
	 if has_trans 
	    	 -- arguably, this should include the new bindings
	    then apply success inter
	    else apply failure inter

-- | do the first rewrite, and the second.

(>+>) :: (Decs dec,Monad m) =>  Translate m dec s1 s2 -> Translate m dec s2 s3 -> Translate m dec s1 s3
before >+> after = before >?> (after,after)

-- | do the first rewrite, then second iff the first changed something.

(>&>) :: (Decs dec,Monad m) =>  Rewrite m dec s -> Rewrite m dec s -> Rewrite m dec s
before >&> after = before >?> (after,nullRewrite)

-- | do the first rewrite, then second iff the first did not
--   changed anything.

(>|>) :: (Decs dec,Monad m) =>  Rewrite m dec s -> Rewrite m dec s -> Rewrite m dec s
before >|> after = before >?> (nullRewrite,after)
     
many :: (Decs dec,Monad m) =>  Rewrite m dec s -> Rewrite m dec s
many rewrite = rewrite >&> (many rewrite)

------------------------------------------------------------------------------
-- catches a failure, replacing it with the identity rewrite.

cut :: (Decs dec,Monad m) =>   Rewrite m dec s -> Rewrite m dec s
cut rr = rewrite $ \ exp -> cutM exp (apply rr exp)

cutM :: (Decs dec,Monad m) =>  s -> RewriteM m dec s -> RewriteM m dec s
cutM s (RewriteM m) = RewriteM $ \ path decs -> do
     r <- m path decs
     case r of
        RewriteSuccessM e' ds -> return (RewriteSuccessM e' ds)
	RewriteReturnM e'            -> return (RewriteReturnM e')
	RewriteFailureM msg      -> return (RewriteReturnM s)

getPathM :: (Decs dec,Monad m) => RewriteM m dec Path
getPathM = RewriteM $ \ path _ -> return $ RewriteSuccessM path mempty

addPathM :: (Decs dec,Monad m) =>  Int -> RewriteM m dec s -> RewriteM m dec s
addPathM ix (RewriteM m) = RewriteM $ \ path dec -> m (path ++ [ix]) dec



--addPath :: (Decs dec,Monad m) =>  Int -> Rewrite m dec s -> Rewrite m dec s
--addPath ix rr = rewrite $ \ exp -> addPathM ix (apply rr exp)
 
{-
-- scoped messages
:: (Monad m) =>  (s -> s -> info) -> Rewrite m dec s -> Rewrite m dec s 
i rr =  \ exp -> RewriteM $ \ path decs -> do 
       	       	         r <- runRewriteM_ (rr exp) path decs
			 case r of
			   RewriteSuccessM exp' infos decs' -> return $ RewriteSuccessM exp' [Node (path,i exp exp') infos] decs' 
			   RewriteReturnM exp'                  -> return $ RewriteSuccessM exp' [Node (path,i exp exp') []] mempty
			   RewriteFailureM msg              -> return $ RewriteFailureM msg

-}

bindingsM :: (Monad m) =>  RewriteM m dec dec
bindingsM = RewriteM $ \ path dec -> return $ RewriteReturnM dec

addBindingsM :: (Decs dec, dec ~ Context a,Monad m) =>  dec -> RewriteM m (Context a) a -> RewriteM m (Context a) a
addBindingsM decs m = RewriteM $ \ path dec -> runRewriteM m path (decs `mappend` dec)

-- This dec *must* have a new name.
liftBindingsM :: (Monad m) =>  dec -> RewriteM m dec ()
liftBindingsM decs = RewriteM $ \ path dec -> return $ RewriteSuccessM () decs

------------------------------------------------------------------------------
{-
data SubstOrder = Prefix Bool -- recurse on the result of any rewrite
     		| Postfix     -- apply subst's after the treewalk
		| Here	      -- only apply in one location
		| Path Int SubstOrder 
		       	      -- dig down a specific path
		deriving (Eq, Ord, Show)

type SubstRewrite m i d e  = SubstOrder -> SubstEnv e -> Rewrite m i d e
type SubstRewriteM m i d e = SubstOrder -> SubstEnv e -> RewriteM m i d e

substRewrite :: (Decs d,Subst s,Monad m,i) => SubstRewrite m i d s
substRewrite order env =
     case order of
       Postfix      -> substInside           >+> thisSubstRewrite env
       Prefix True  -> thisSubstRewrite env  >+> substInside
       Prefix False -> thisSubstRewrite env  >|> substInside
       Here         -> thisSubstRewrite env
       Path {}	    -> substInside
  where
    substInside = undefined -- rewrite (\ s -> substOver (substInsideNode s) order env)




class Subst e where
  data SubstEnv e 
  -- split the tree into sub-components, that can be themselves walked
  substInsideNode :: e -> Cons e
  
-- class Subst' s where
  -- a local rewrite, based on *this* node
  thisSubstRewrite :: SubstEnv e -> Rewrite m i d e

--substOver :: (Decs d,Subst e,Monad m,i) => Node e -> SubstRewriteM m i d e
--substOver node = substOver' (depth node) node

--substOver' :: (Decs d,Subst e,Monad m,i) => Int -> Node e -> SubstRewriteM m i d e
{-
substOver' :: (Decs d,Monad m,i) => Int -> Node e -> SubstOrder -> SubstEnv e 
           -> (forall b e . SubstEnv e -> SubstEnv (b -> e))
           -> RewriteM m i d e
substOver' n (Cons a)      order env abc = return a
substOver' n (node :. arg) order env abc = do 
  f <- substOver' (pred n) node order (abc env) abc
  return (f arg)

-}
{-
substOver' n (node :* arg) order env = do
  f <- substOver' (pred n) node order env
--  arg' <- substOn n arg order env
--  return (f arg')
  return undefined
-}
{-
substOverX ::  (Decs d,Monad m,i) =>
               ( forall b. Int -> Node (b -> e) -> RewriteM m i d (b -> e) )
           ->  Int
           ->  Node e
           -> RewriteM m i d e              
substOverX fn n (Cons a)       = return a
substOverX fn n (node :. arg)  = do  
  f <- fn (pred n) node  
  undefined
-}
{-
  return (f undefined) --  arg)
-}

substOn :: (Decs d,Subst e,Monad m,i) => Int -> e -> SubstRewriteM m i d e
substOn _ e (Here {})      env = return e
substOn i e (Path j order) env 
        | i == j                       = apply (substRewrite order env) e
        | otherwise                    = return e
substOn i e order          env     = apply (substRewrite order env) e
-}
-------------------------------------------------------------------------------

class Term exp where
  type Generic exp
  type Context exp

  explodeCons :: exp -> Cons exp

  -- everything follows from these
  project :: (Monad m) => Generic exp -> m exp
  inject  :: exp -> Generic exp

instance Term b => Term (a -> b) where
    type Generic (a -> b) = Generic b
    type Context (a -> b) = Context b

extract  :: (Term exp, Monad m, Decs dec, dec ~ Context exp) => Rewrite m dec (Generic exp) -> Rewrite m dec exp	-- at *this* type
extract rr = rewrite $ \ e -> do
            e' <- apply rr (inject e)
            project e'

-- promote a rewrite into a generic rewrite
-- other types are fails.
package  :: (Term exp, Monad m, Decs dec) => Rewrite m dec exp -> Rewrite m dec (Generic exp)
package rr = rewrite $ \ e -> do
               e' <- project e
               r <- apply rr e'
               return (inject r)

applyG :: (Term exp,Monad m,Decs dec,dec ~ Context exp) => Rewrite m dec (Generic exp) -> exp -> RewriteM m dec exp
applyG exp = apply (extract exp)

------------------------------------------------------------------------------

data Generics
	= GExp Exp
	| GRoot Root

data Exp = Exp Int Exp Root

instance Term Exp where
  type Generic Exp = Generics
  type Context Exp = ()

  explodeCons (Exp n e r) = Cons Exp 
                            :. n 
                            :* e 
                            :** (Scoped () r)

  inject = GExp

  project (GExp e) = return e
  project _        = fail "project of non-GExp"

data Root = Root Exp

instance Term Root where
  type Generic Root = Generics
  type Context Root = ()

  explodeCons (Root e) = Cons Root :* e 

  inject = GRoot

  project (GRoot e) = return e
  project _         = fail "project of non-GRoot"

-------------------------------------------------------------------------------
-- Rename to Shape
data Cons a = Cons a
            | forall b .  (Cons (b -> a)) :. b
            | forall b . (Term b,Generic b ~ Generic a, Context b ~ Context a) => (Cons (b -> a)) :* b
            | forall b . (Term b,Generic b ~ Generic a,Context b ~ Context a,Decs (Context b)) => (Cons (b -> a)) :** Scoped b

data Scoped b = Scoped (Context b) b

all :: (Term exp, Monad m, Decs dec, dec ~ Context exp) 
       => Rewrite m (Context exp) (Generic exp) 
       -> Rewrite m (Context exp) exp
all rr = rewriteWithId $ \ e -> liftM fst $ rewriteChildren rr (explodeCons e)

rewriteChildren :: (Decs d,Monad m,Term e, d ~ Context e) 
    => (Rewrite m d (Generic e))
    -> Cons e 
    -> RewriteM m d (e,Int)
rewriteChildren gen (Cons a) = return (a,0)
rewriteChildren gen (fn :. b) = do
  (f,n) <- rewriteChildren gen fn
  return (f b,succ n)
rewriteChildren gen (fn :* b) = do
  (f,n) <- rewriteChildren gen fn 
  b <- addPathM n $ apply (extract gen) b
  return (f b,succ n)
rewriteChildren gen (fn :** (Scoped dec b)) = do
  (f,n) <- rewriteChildren gen fn 
  b <- addPathM n $ addBindingsM dec $ apply (extract gen) b
  return (f b,succ n)

infixl 3 :., :*, :**

------------------------------------------------------------------------------

class (Monoid dec) => Decs dec where
  type Key dec
  type Dec dec
-- these two are from the monoid
--  emptyDecs  :: dec
--  mergeDecs   :: dec -> dec -> dec
  lookupDecs :: Key dec -> dec -> Dec dec
  addDec     :: Key dec -> Dec dec -> dec -> dec

instance Decs () where {}

{-
class (Monoid info) => where
  type InfoElem 
  unit:: Path -> InfoElem info -> info
  anonInfo :: Path -> info

instance Info () where {}
-}

--bottomup :: (Term s, Info info, Monad m, Decs dec) =>
--            Rewrite m info dec (Generic s) -> Rewrite m info dec (Generic s)
bottomup s = package (Language.KURE.Rewrite.all (bottomup s) >&> extract s)

-- all s = children (package s)

