{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
-- |
-- Module: Language.KURE.Types
-- Copyright: (c) 2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- This is the definition of the types inside KURE.

-- module Language.KURE.Types where
module Types where

import Prelude hiding (catch)
import System.IO.Error hiding (catch)
import Control.Exception (catch)

import Control.Applicative
import Control.Monad
import Data.Pointed
import Data.Copointed
import Data.Monoid

------------------------------------------------------------------------------------------

-- | 'Translate' is a translation or strategy that translates from a value in a context to a monadic value.
data Translate c m a b = Translate {apply :: c a -> m b}

-- | 'translate' is the standard way of building a 'Translate'.
translate :: (c a -> m b) -> Translate c m a b
translate = Translate

instance Functor m => Functor (Translate c m a) where
  
-- fmap :: (b -> d) -> Translate c m a b -> Translate c m a d  
   fmap f t = translate (fmap f . apply t)

instance Pointed m => Pointed (Translate c m a) where
-- point :: b -> Translate c m a b
   point b = translate (\ _ -> point b)

instance Applicative m => Applicative (Translate c m a) where
  
-- pure :: b -> Translate c m a b  
   pure b = translate (\ _ -> pure b)
   
-- (<*>) :: Translate c m a (b -> d) -> Translate c m a b -> Translate c m a d   
   tf <*> tb = translate (\ ca -> apply tf ca <*> apply tb ca) 

instance Monad m => Monad (Translate c m a) where 
  
-- return :: b -> Translate c m a b
   return b = translate (\ _ -> return b)
-- (>>=) :: Translate c m a b -> (b -> Translate c m a d) -> Translate c m a d  
   
   tb >>= f = translate $ \ ca -> do b <- apply tb ca 
                                     apply (f b) ca
                                     
-- fail :: String -> Translate c m a b
   fail msg = translate (\ _ -> fail msg)

instance MonadPlus m => MonadPlus (Translate c m a) where
-- mzero :: Translate c m a b  
   mzero = translate (\ _ -> mzero)

-- mplus :: Translate c m a b -> Translate c m a b -> Translate c m a b
   mplus t1 t2 = translate (\ ca -> apply t1 ca `mplus` apply t2 ca)

-- | 'Pointed' should be a superclass of 'Applicative', 
--   and 'Applpicative' should be a superclass of 'Monad'.
--   They aren't, but we can make them superclasses of 'MonadCatch'.
class (Pointed m, Applicative m, Monad m) => MonadCatch m where
-- | Catch monadic 'fail's  
   catchM :: m a -> (String -> m a) -> m a

instance MonadCatch m => MonadCatch (Translate c m a) where
-- catchM :: Translate c m a b -> (String -> Translate c m a) -> Translate c m a
   catchM t f = translate (\ ca -> apply t ca `catchM` (\ msg -> apply (f msg) ca))
  
instance MonadCatch Maybe where
-- catchM :: Maybe a -> (String -> Maybe a) -> Maybe a
   catchM (Just a)  f = Just a  
   catchM (Nothing) f = f "Failure inside Maybe Monad"

instance MonadCatch (Either String) where
-- catchM :: Either String a -> (String -> Either String a) -> Either String a
   catchM (Right a)  f = Right a  
   catchM (Left msg) f = f msg

instance MonadCatch IO where
-- catchM :: IO a -> (String -> IO a) -> IO a  
   catchM ma f = ma `catch` (\ e -> if isUserError e
	 	                     then f $! (ioeGetErrorString e)
		                     else ioError e)
                 
-- | 'EndoFunctor' is a Functor that only allows the mapping of endofunctions
--   This is different from the categorical notion of an endofunctor, 
--   which is a functor from a category to itself (and which all 'Functor's in Haskell are anyway).   
class EndoFunctor c where   
  liftC :: (a -> a) -> c a -> c a

class (Copointed c, EndoFunctor c) => EndoCopointed c where

constC :: EndoFunctor c => a -> c a -> c a
constC a = liftC (const a) 

-- | Replace the value in the context while leaving the context unchanged
replaceC :: EndoFunctor c => c a -> a -> c a
replaceC = flip constC

lowerC :: Copointed c => (a -> b) -> c a -> b
lowerC f = f . copoint

lowerC2 :: Copointed c => (a -> b -> d) -> c a -> c b -> d
lowerC2 f ca cb = f (copoint ca) (copoint cb)

------------------------------------------------------------------------------------------

-- | A 'Rewrite' is a 'Translate' that shares the same source and target type.
type Rewrite c m a = Translate c m a a

-- | 'rewrite' is our primitive way of building a Rewrite
rewrite :: (c a -> m a) -> Rewrite c m a
rewrite = translate
  
------------------------------------------------------------------------------------------

-- | 'Term's are things that syntax are built from.
class (Generic a ~ Generic (Generic a)) => Term a where
  
  -- | 'Generic' is a sum of all the interesting sub-types, transitively, of @a@.
  -- We use @Generic a ~ a@ to signify that something is its own Generic.
  -- Simple expression types might be their own sole 'Generic', more complex examples
  -- will have a new datatype for the 'Generic', which will also be an instance of class 'Term'.
  type Generic a

  -- | 'inject' injects an expression into a 'Generic'.
  inject :: a -> Generic a

  -- | 'select' attempts to extract an expression of the correct type from a 'Generic'.
  select :: Generic a -> Maybe a


-- | 'TermC's are 'Term's in a context
class (EndoFunctor c, Term a) => TermC c a where
  
-- | 'injectC' allows an expression to be injected into a 'Generic' inside a context.
  injectC :: c a -> c (Generic a)
  
  -- | 'selectC' attempts to extract an expression of the correct type from a 'Generic', inside a context.
  selectC :: c (Generic a) -> Maybe (c a)
  

-- | 'Walker' captures how we walk over an expression in a context, using a monad m. 
class (TermC c a, Monad m) => Walker c m a where

  -- | 'allR' applies 'Generic' rewrites to all the interesting children of this node.
  allR :: Rewrite c m (Generic a) -> Rewrite c m a

  -- | 'crushT' applies a 'Generic' Translate to a common, 'Monoid'al result, to all the interesting children of this node.
  crushT :: (Monoid b) => Translate c m (Generic a) b -> Translate c m a b
  
------------------------------------------------------------------------------------------
