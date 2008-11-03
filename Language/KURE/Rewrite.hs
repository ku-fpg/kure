{-# LANGUAGE ExistentialQuantification, TypeFamilies, Rank2Types, MultiParamTypeClasses  #-}
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
       , Path
       , bindingsM
       , addBindingsM
       , liftBindingsM
       , Node(..) 
       , Subst(..)
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

newtype Rewrite m info dec exp = 
    Rewrite { unRewrite :: exp -> Path -> dec -> m (RewriteStatus info dec exp) }

-- A rewrite where we have already provided the input expression,
-- can return an identity 

data RewriteM m info dec exp = 
   RewriteM { runRewriteM_ :: Path -> dec -> m (RewriteStatusM info dec exp) }

-- Three modes
--   * success, with changes, and perhaps new scoping bindings.
--   * success, with no location (yet)
--   * failure (exception, like pattern match failure)

data RewriteStatus info dec exp 
     = RewriteSuccess exp info dec
       		      	  		-- always at least 1 info,
                                        -- decs are only *new* decs
     | RewriteFailure String		-- a real failure
     | RewriteChangeless		-- nothing changed

data RewriteStatusM info dec exp
     = RewriteSuccessM exp info dec
       		      	  		-- always at least 1 info,
                                        -- decs are only *new* decs
     | RewriteReturnM exp		-- unmarked success
     | RewriteFailureM String		-- a real failure


apply :: (Monad m) => Rewrite m info dec exp -> exp -> RewriteM m info dec exp
apply (Rewrite action) exp = RewriteM $ \ path decs -> do
  r <- action exp path decs
  case r of
    RewriteSuccess exp info dec -> return $ RewriteSuccessM exp info dec
    RewriteFailure msg		-> return $ RewriteFailureM msg
    RewriteChangeless           -> return $ RewriteReturnM exp	-- original exp

-- This promotes a monadic action into a Rewrite. Any non-failure is turned into
-- succeess, with a location tag.

-- so: rewrite $ \ (Add (Val x) (Val y)) -> return (Var (x + y))
-- is marked as a rewritten

rewrite :: (Monad m,Info info,Monoid dec) => (exp -> RewriteM m info dec exp) -> Rewrite m info dec exp
rewrite action = Rewrite $ \ exp path dec -> do
  r <- runRewriteM_ (action exp) path dec
  case r of
    RewriteSuccessM exp info dec -> return $ RewriteSuccess exp info dec
    RewriteFailureM msg		 -> return $ RewriteFailure msg
    RewriteReturnM exp	         -> return $ RewriteSuccess exp (anonInfo path) mempty


-- precondition, if RewriteM returns, it returns with the same value as given.
-- one way of knowning this is if the only things that could change the structure
-- are sub-calls to RewriteM.

rewriteWithId :: (Monad m,Info info,Monoid dec) => (exp -> RewriteM m info dec exp) -> Rewrite m info dec exp
rewriteWithId action = Rewrite $ \ exp path dec -> do
  r <- runRewriteM_ (action exp) path dec
  case r of
    RewriteSuccessM exp info dec -> return $ RewriteSuccess exp info dec
    RewriteFailureM msg		 -> return $ RewriteFailure msg
    RewriteReturnM _exp	         -> return $ RewriteReturnM exp	-- assert: _exp == exp

-- The simple form, unability to rewrite in localize as an Null
-- exposing interface, perhaps the bindings listing will change

runRewrite :: (Decs dec,Monad m,Monoid info) =>  Rewrite m info dec exp 
	   -> Path 
	   -> dec 
	   -> exp 
	   -> m (exp,info,dec)
runRewrite rr path decs exp = do
  res <- runRewriteM_ (apply rr exp) path decs
  case res of
     RewriteSuccessM exp' is ds -> return (exp',is,ds)
     RewriteReturnM exp             -> return (exp,mempty,mempty)
     RewriteFailureM msg        -> fail msg


nullRewrite :: (Decs dec,Monad m,Info info) =>  Rewrite m info dec exp
nullRewrite =  Rewrite $ \ _ _ _ -> return (RewriteChangeless)

instance (Decs dec,Monad m,Monoid info) => Monad (RewriteM m info dec) where
   return exp = RewriteM $ \ _path _dec -> return $ RewriteReturnM exp
   (RewriteM m) >>= k = RewriteM $ \ path dec -> do
   	     	      		 r <- m path dec
				 case r of
				   RewriteSuccessM r is ds -> do
				     r' <- runRewriteM_ (k r) path (ds `mappend` dec)
				     return $ 
				      case r' of
				       RewriteSuccessM e' is' ds'
				       		         -> RewriteSuccessM e' 
							    		   (is `mappend` is') 
									   (ds `mappend` ds')
				       RewriteReturnM e' -> RewriteSuccessM e' is ds
				       RewriteFailureM msg -> RewriteFailureM msg
				   RewriteReturnM r -> do
				     r' <- runRewriteM_ (k r) path dec
				     return $
				      case r' of
				       RewriteSuccessM e' is' ds'
				       		         -> RewriteSuccessM e' 
							    		   is' 
									   ds'
				       RewriteReturnM e' -> RewriteReturnM e'
				       RewriteFailureM msg -> RewriteFailureM msg
				   RewriteFailureM msg -> return $ RewriteFailureM msg
   fail msg = RewriteM $ \ _ _ -> return $ RewriteFailureM msg

liftQ :: (Monad m) =>  m a -> RewriteM m info dec a   
liftQ m = RewriteM $ \ _ _ -> do r <- m
      	  	       	         return $ RewriteReturnM r

instance (Decs dec,Monad m,Info info) => Functor (RewriteM m info dec) where
  fmap f m = liftM f m

------------------------------------------------------------------------------

testSuccessM :: (Decs dec,Monad m) =>  RewriteM m info dec a -> RewriteM m info dec (a,Bool)
testSuccessM (RewriteM m) = RewriteM $ \ path dec -> do
	    r <- m path dec
	    case r of
	       RewriteSuccessM e' is ds -> return (RewriteSuccessM (e',True) is ds)
	       RewriteReturnM e'            -> return (RewriteReturnM (e',False))
	       RewriteFailureM msg      -> return (RewriteFailureM msg)

-- small messages
messageM :: (Decs dec,Monad m,Info info) => InfoElem info -> RewriteM m info dec ()
messageM info = RewriteM $ \ path dec -> return $ RewriteSuccessM () (unitInfo path info) mempty

(>?>) :: (Decs dec,Monad m,Info info) =>  Rewrite m info dec s  
      -> (Rewrite m info dec s,Rewrite m info dec s) 
      -> Rewrite m info dec s
pred >?> (success,failure) = rewrite $ \ src -> do
     	 (inter,has_trans) <- testSuccessM (apply pred src)
	 if has_trans 
	    	 -- arguably, this should include the new bindings
	    then apply success inter
	    else apply failure inter

-- | do the first rewrite, and the second.

(>+>) :: (Decs dec,Monad m,Info info) =>  Rewrite m info dec s -> Rewrite m info dec s -> Rewrite m info dec s
before >+> after = before >?> (after,after)

-- | do the first rewrite, then second iff the first changed something.

(>&>) :: (Decs dec,Monad m,Info info) =>  Rewrite m info dec s -> Rewrite m info dec s -> Rewrite m info dec s
before >&> after = before >?> (after,nullRewrite)

-- | do the first rewrite, then second iff the first did not
--   changed anything.

(>|>) :: (Decs dec,Monad m,Info info) =>  Rewrite m info dec s -> Rewrite m info dec s -> Rewrite m info dec s
before >|> after = before >?> (nullRewrite,after)
     
many :: (Decs dec,Monad m,Info info) =>  Rewrite m info dec s -> Rewrite m info dec s
many rewrite = rewrite >&> (many rewrite)

------------------------------------------------------------------------------
-- catches a failure, replacing it with the identity rewrite.

cut :: (Decs dec,Monad m,Info info) =>   Rewrite m info dec s -> Rewrite m info dec s
cut rr = rewrite $ \ exp -> cutM exp (apply rr exp)

cutM :: (Decs dec,Monad m,Info info) =>  s -> RewriteM m info dec s -> RewriteM m info dec s
cutM s (RewriteM m) = RewriteM $ \ path decs -> do
     r <- m path decs
     case r of
        RewriteSuccessM e' is ds -> return (RewriteSuccessM e' is ds)
	RewriteReturnM e'            -> return (RewriteReturnM e')
	RewriteFailureM msg      -> return (RewriteReturnM s)

getPathM :: (Decs dec,Monad m,Info info) => RewriteM m info dec Path
getPathM = RewriteM $ \ path _ -> return $ RewriteSuccessM path mempty mempty

addPathM :: (Decs dec,Monad m,Info info) =>  Int -> RewriteM m info dec s -> RewriteM m info dec s
addPathM ix (RewriteM m) = RewriteM $ \ path dec -> m (path ++ [ix]) dec

addPath :: (Decs dec,Monad m,Info info) =>  Int -> Rewrite m info dec s -> Rewrite m info dec s
addPath ix rr = rewrite $ \ exp -> addPathM ix (apply rr exp)

{-
-- scoped messages
info :: (Monad m,Info info) =>  (s -> s -> info) -> Rewrite m info dec s -> Rewrite m info dec s 
info i rr =  \ exp -> RewriteM $ \ path decs -> do 
       	       	         r <- runRewriteM_ (rr exp) path decs
			 case r of
			   RewriteSuccessM exp' infos decs' -> return $ RewriteSuccessM exp' [Node (path,i exp exp') infos] decs' 
			   RewriteReturnM exp'                  -> return $ RewriteSuccessM exp' [Node (path,i exp exp') []] mempty
			   RewriteFailureM msg              -> return $ RewriteFailureM msg

-}

bindingsM :: (Monad m,Info info) =>  RewriteM m info dec dec
bindingsM = RewriteM $ \ path dec -> return $ RewriteReturnM dec

addBindingsM :: (Decs dec,Monad m,Info info) =>  dec -> RewriteM m info dec a -> RewriteM m info dec a
addBindingsM decs m = RewriteM $ \ path dec -> runRewriteM_ m path (decs `mappend` dec)

-- This dec *must* have a new name.
liftBindingsM :: (Monad m,Info info) =>  dec -> RewriteM m info dec ()
liftBindingsM decs = RewriteM $ \ path dec -> return $ RewriteSuccessM () mempty decs

------------------------------------------------------------------------------

data SubstOrder = Prefix Bool -- recurse on the result of any rewrite
     		| Postfix     -- apply subst's after the treewalk
		| Here	      -- only apply in one location
		| Path Int SubstOrder 
		       	      -- dig down a specific path
		deriving (Eq, Ord, Show)

type SubstRewrite m i d e  = SubstOrder -> SubstEnv e -> Rewrite m i d e
type SubstRewriteM m i d e = SubstOrder -> SubstEnv e -> RewriteM m i d e

substRewrite :: (Decs d,Subst s,Monad m,Info i) => SubstRewrite m i d s
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
  substInsideNode :: e -> Node e
  
-- class Subst' s where
  -- a local rewrite, based on *this* node
  thisSubstRewrite :: SubstEnv e -> Rewrite m i d e

--substOver :: (Decs d,Subst e,Monad m,Info i) => Node e -> SubstRewriteM m i d e
--substOver node = substOver' (depth node) node

--substOver' :: (Decs d,Subst e,Monad m,Info i) => Int -> Node e -> SubstRewriteM m i d e
substOver' :: (Decs d,Monad m,Info i) => Int -> Node e -> SubstOrder -> SubstEnv e 
           -> (forall b e . SubstEnv e -> SubstEnv (b -> e))
           -> RewriteM m i d e
substOver' n (Cons a)      order env abc = return a
substOver' n (node :. arg) order env abc = do 
  f <- substOver' (pred n) node order (abc env) abc
  return (f arg)
{-
substOver' n (node :* arg) order env = do
  f <- substOver' (pred n) node order env
--  arg' <- substOn n arg order env
--  return (f arg')
  return undefined
-}

substOverX ::  (Decs d,Monad m,Info i) =>
               ( forall b. Int -> Node (b -> e) -> RewriteM m i d (b -> e) )
           ->  Int
           ->  Node e
           -> RewriteM m i d e              
substOverX fn n (Cons a)       = return a
substOverX fn n (node :. arg)  = do  
  f <- fn (pred n) node  
  undefined
{-
  return (f undefined) --  arg)
-}

substOn :: (Decs d,Subst e,Monad m,Info i) => Int -> e -> SubstRewriteM m i d e
substOn _ e (Here {})      env = return e
substOn i e (Path j order) env 
        | i == j                       = apply (substRewrite order env) e
        | otherwise                    = return e
substOn i e order          env     = apply (substRewrite order env) e

-------------------------------------------------------------------------------

class XX exp where
  type Generic exp

  children :: (Info info, Monad m, Decs dec) => Rewrite m info dec (Generic exp) -> Rewrite m info dec exp

  -- everything follows from these
  project :: (Monad m) => Generic exp -> m exp
  inject  :: exp -> Generic exp

extract  :: (XX exp,Info info, Monad m, Decs dec) => Rewrite m info dec (Generic exp) -> Rewrite m info dec exp	-- at *this* type
extract rr = rewrite $ \ e -> do
            e' <- apply rr (inject e)
            project e'

-- promote a rewrite into a generic rewrite
-- other types are fails.
package  :: (XX exp,Info info, Monad m, Decs dec) => Rewrite m info dec exp -> Rewrite m info dec (Generic exp)
package rr = rewrite $ \ e -> do
               e' <- project e
               r <- apply rr e'
               return (inject r)

--fmap2 :: (a -> b) -> (b -> a) -> Rewrite m info dec a -> Rewrite m info dec b
--fmap2 ::

applyG :: (XX exp,Monad m,Decs dec,Info info) => Rewrite m info dec (Generic exp) -> exp -> RewriteM m info dec exp
applyG exp = apply (extract exp)
{-
class XX exp where
  type GenericRewrite exp

  children :: (Info info, Monad m, Decs dec) => (GenericRewrite exp) -> Rewrite m info dec exp

  extract  :: (Info info, Monad m, Decs dec) => (GenericRewrite exp) -> Rewrite m info dec exp	-- at this type
  package  :: (Info info, Monad m, Decs dec) => Rewrite m info dec exp -> GenericRewrite exp	-- build a 

  apply   
--  promote :: Rewrite m info dec exp -> 


class (XX exp) => YY rrs exp where
   mkGenericRewrite :: rrs -> GenericRewrite exp

{-
`-- This rewrites all the *children* of any exp node.
children :: (XX exp) => Rewrite m info dec (GenericRewrite exp) -> Rewrite m info dec exp
children = undefined
-}

{- 
 - Want some way of joining various things into a GenericRewrite exp
 -}


--data Rewrites m info dec = Rewrites
--    (Rewrite m info dec Exp)
--    (Rewrite m info dec Root)
-}

data Generics
	= GExp Exp
	| GRoot Root

{-
data Rewrites = Rewrites
    (forall m info dec . (Info info, Monad m, Decs dec) => Rewrite m info dec Exp)
    (forall m info dec . (Info info, Monad m, Decs dec) => Rewrite m info dec Root)

--idGenericRewrite :: GenericRewrite a	--
idGenericRewrite = Rewrites (nullRewrite) (nullRewrite)


--joinCO :: (forall m info dec . Rewrite m info dec Exp) -> (GenericRewrite b) -> GenericRewrite c
--joinCO (Rewrites 
-}

data Exp = Exp Int Exp Root

instance XX Exp where
  type Generic Exp = Generics

  children gen = rewrite $ \ (Exp i exp root) -> do
                exp'  <- applyG gen exp
                root' <- applyG gen root
                return (Exp i exp' root')

  inject = GExp

  project (GExp e) = return e
  project _        = fail "project of non-GExp"

data Root = Root Exp

instance XX Root where
  type Generic Root = Generics
{-
  children (Rewrites rrExp rrRoot) = rewrite $ \ (Root exp) -> do
                exp'  <- apply rrExp exp
                return (Root exp')
-}

{- class SubTypes exp where
  :: Rewrite Generic -> GenericRewrite exp
 -}

-------------------------------------------------------------------------------

data Node a = Cons a
            | forall b .              (Node (b -> a)) :. b
            | forall b . (Subst b) => (Node (b -> a)) :* b
--            | forall b . (Subst b) => (Node (b -> a)) :** Scoped b

depth :: Node a -> Int
depth (Cons a)    = 0
depth (node :. _) = depth node + 1
depth (node :* _) = depth node + 1

-- data Scoped b = Scoped b (

infixl 3 :., :* --,:**

{-
data Exp = Var String | App Exp Exp | Lam String Exp

instance Subst Exp where {}

cons :: a -> Node a
cons = Cons

--tree :: Node a -> Tree ()
--tree (

foo :: Exp -> Node Exp
foo (Var exp)   = cons Var :. exp
foo (App e1 e2) = cons App :. e1 :. e1
--foo (Lam n e)   =  cons Lam :. n  :** (Binding  e
-}
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


class (Monoid info) => Info info where
  type InfoElem info 
  unitInfo :: Path -> InfoElem info -> info
  anonInfo :: Path -> info

instance Info () where {}

--bottomup :: (XX s, Info info, Monad m, Decs dec) =>
--            Rewrite m info dec s -> Rewrite m info dec s
bottomup s = children (package (bottomup s)) >&> s

all s = children (package s)

