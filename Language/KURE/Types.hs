{-# LANGUAGE TypeFamilies, ExistentialQuantification, Rank2Types #-}
-- |
-- Module: Language.KURE.Types
-- Copyright: (c) 2010 The University of Kansas
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- This is the definition of the types inside KURE.

module Language.KURE.Types where 

import System.IO.Error
import Data.Monoid
import qualified Control.Category as Cat
import Control.Arrow

-- | 'Translate' is a translation or strategy that translates between @exp1@ and @exp2@, with the posiblity of failure,
-- and remembers identity translations.

data Translate exp1 exp2 = Translate (forall m . (TranslateMonad m) => exp1 -> m exp2)

-- | 'apply' directly applies a 'Translate' value to an argument.
apply :: (TranslateMonad m) => Translate exp1 exp2 -> exp1 -> m exp2
apply (Translate t) exp1 = t exp1 

-- | 'translate' is the standard way of building a 'Translate', where if the translation is successful it 
-- is automatically marked as a non-identity translation. 

translate :: (forall m . (TranslateMonad m) => exp1 -> m exp2) -> Translate exp1 exp2
translate f = Translate $ \ e -> f e

instance Cat.Category Translate where
   id = translate $ return 
   (.) rr1 rr2 = translate $ \ e -> apply rr2 e >>= apply rr1

instance Arrow Translate where
   arr f = translate (return Prelude.. f)
   first rr = translate $ \ (b,d) -> do c <- apply rr b ; return (c,d)

-- | A 'Rewrite' is a 'Translate' that shares the same source and target type. Literally, 
-- a 'Rewrite' provides the details about how to /re-write/ a specific type.

type Rewrite exp = Translate exp exp

-- | 'rewrite' is our primitive way of building a Rewrite,
--
-- @rewrite $ \\ _ e -> return e@ /is/ (now) an identity rewrite. 

rewrite :: (forall m . (TranslateMonad m) => exp1 -> m exp1) -> Rewrite exp1
rewrite = translate

-- | 'TranslateMonad' is the monad inside translate. You can define your own, or use 
-- 'IO' or 'Maybe'.

class Monad m => TranslateMonad m where
	catchTM :: m a -> (String -> m a) -> m a 
	ptrEqualsTM :: a -> a -> m Bool
	ptrEqualsTM x y = return $ False

instance TranslateMonad IO where
	catchTM m1 m2 = m1 `Prelude.catch` (\ e ->
		if isUserError e 
		then m2 $! (ioeGetErrorString e)
		else ioError e)

instance TranslateMonad Maybe where
	catchTM m1 m2 = case m1 of
		Nothing -> m2 "fail inside Maybe monad"
		Just v -> Just v

-- | 'Term's are things that syntax are built from.
class Eq exp => Term exp where
  -- | 'Generic' is a sum of all the interesting sub-types, transitively, of @exp@. 
  -- We use @Generic e ~ e@ to signify that something is its own Generic.
  -- Simple expression types might be their own sole 'Generic', more complex examples
  -- will have a new datatype for the 'Generic', which will also be an instance of class 'Term'.
  type Generic exp

  -- | 'project' projects into a 'Generic', to get the exp inside, or fails.
  select :: Generic exp -> Maybe exp

  -- | 'inject' injects an exp into a 'Generic'.
  inject  :: exp -> Generic exp

  -- | 'equals' creates an predicate 'Translate' specialized to the first argument.
  equals :: exp -> Translate exp Bool
  equals e0 = Translate $ \ e1 -> do
		b <- ptrEqualsTM e0 e1
		return $ b || e0 == e1

  -- | 'allR' applies 'Generic' rewrites to all the interesting children of this node.
  allR :: Rewrite (Generic exp) -> Rewrite exp
  -- | 'allU' applied a 'Generic' Translation to a common, 'Monoid'al result, to all the interesting children of this node.
  crushU :: (Monoid result) => Translate (Generic exp) result -> Translate exp result

{-
  type Tag exp
  tag :: exp -> Tag exp
-}

--type RewritePath exp = Rewrite (Generic exp) -> Rewrite exp
--RewritePath exp


