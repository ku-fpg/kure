{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
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
   
-- | 'Applicative' should be a superclass of 'Monad'.
--   It isn't, but we partially compensate for it by making it a superclass of 'MonadCatch'.
class (Applicative m, Monad m) => MonadCatch m where
  
-- | Left Biased mplus  
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
                 
-- | Contextual is similar to comonad, but more restricted.
--   It's precise definition is still under development. 
--   It's very close to a co-endomonad, but that's not quite good enough due to the required interaction with monads. 
class Contextual c where
  extractC :: c a -> a
  replaceC :: c a -> a -> c a  

lowerC :: Contextual c => (a -> b) -> c a -> b
lowerC f = f . extractC

lowerC2 :: Contextual c => (a -> b -> d) -> c a -> c b -> d
lowerC2 f ca cb = f (extractC ca) (extractC cb)

liftC :: Contextual c => (a -> a) -> c a -> c a
liftC f ca = replaceC ca (lowerC f ca)

------------------------------------------------------------------------------------------

-- | A 'Rewrite' is a 'Translate' that shares the same source and target type.
type Rewrite c m a = Translate c m a a

-- | 'rewrite' is our primitive way of building a Rewrite
rewrite :: (c a -> m a) -> Rewrite c m a
rewrite = translate

------------------------------------------------------------------------------------------

-- | 'Term's are things that syntax are built from.
class Eq exp => Term exp where
  
  -- | 'Generic' is a sum of all the interesting sub-types, transitively, of @exp@.
  -- We use @Generic e ~ e@ to signify that something is its own Generic.
  -- Simple expression types might be their own sole 'Generic', more complex examples
  -- will have a new datatype for the 'Generic', which will also be an instance of class 'Term'.
  type Generic exp

  -- | 'select' looks in a 'Generic', to get the exp inside, or fails.
  select :: Generic exp -> Maybe exp

  -- | 'inject' injects an exp into a 'Generic'.
  inject :: exp -> Generic exp

  -- | 'allR' applies 'Generic' rewrites to all the interesting children of this node.
  allR :: Rewrite c m (Generic exp) -> Rewrite c m exp

  -- | 'crushU' applies a 'Generic' Translate to a common, 'Monoid'al result, to all the interesting children of this node.
  crushU :: (Monoid b) => Translate c m (Generic exp) b -> Translate c m exp b
  