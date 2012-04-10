-- |
-- Module: Language.KURE.Translate
-- Copyright: (c) 2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- This module defines the main KURE types: 'Translate' and 'Rewrite'.
-- It also contains various combinators that operate over these types. The convention is that
-- 'Translate' based combinators end with @T@, and 'Rewrite' based combinators end with @R@. Of course,
-- because 'Rewrite' is a type synonymm of 'Translate', the 'Rewrite' functions also operate with on 'Translate',
-- and the 'Translate' functions operate with 'Rewrite'.

module Language.KURE.Translate
       (  -- * Primitives
          Translate
        , Rewrite
        , apply  
        , translate  
        , rewrite
          -- * The 'Translate' combinators
        , (<+)
        , (>->)
        , failT
        , contextT
        , liftT
        , constT
        , readerT
        , concatT
        , emptyT
          -- * The 'Rewrite' combinators
        , idR
        , tryR
        , repeatR
        , acceptR
        , (?)
          -- * The prelude combinators          
        , tuple2R
        , listR
        , maybeR
        , tuple2T
        , listT
        , maybeT
        , fromJustT  
) where

import Prelude hiding (id, (.))
import Data.Monoid
import Data.Pointed
import Data.Traversable
import Control.Applicative
import Control.Category
import Control.Arrow

infixl 3 <+, >->
infixr 3 ?

------------------------------------------------------------------------------------------

-- | 'Translate' is a translation or strategy that translates from a value in a context to a monadic value.
data Translate c m a b = Translate {apply :: c -> a -> m b}

-- | A 'Rewrite' is a 'Translate' that shares the same source and target type.
type Rewrite c m a = Translate c m a a

-- | 'translate' is the standard way of building a 'Translate'.
translate :: (c -> a -> m b) -> Translate c m a b
translate = Translate

-- | 'rewrite' is our primitive way of building a 'Rewrite'.
rewrite :: (c -> a -> m a) -> Rewrite c m a
rewrite = translate

------------------------------------------------------------------------------------------

-- | identity rewrite.
idR :: Pointed m => Rewrite c m a
idR = rewrite (\ _ -> point)
      
-- | extract the current context
contextT :: Pointed m => Translate c m a c
contextT = translate (\ c _ -> point c)

-- | lift a function into a 'Translate'
liftT :: Pointed m => (a -> b) -> Translate c m a b
liftT f = translate (\ _ -> point . f)

-- | 'constT' produces an unfailable 'Translate' that returns the first argument.
constT :: Pointed m => b -> Translate c m a b
constT b = translate (\ _ _ -> point b)

-- | failing translation.
failT :: Alternative m => Translate c m a b
failT = translate (\ _ _ -> empty)

-- | like a catch, '<+' does the first 'Translate', and if it fails, then does the second 'Translate'.
(<+) :: Alternative m => Translate c m a b -> Translate c m a b -> Translate c m a b
t1 <+ t2 = translate $ \ c a -> apply t1 c a <|> apply t2 c a

-- | sequencing translates
(>->) :: Monad m => Translate c m a b -> Translate c m b d -> Translate c m a d
t1 >-> t2 = translate $ \ c a -> apply t1 c a >>= apply t2 c

firstT :: Functor m => Translate c m a b -> Translate c m (a,z) (b,z)
firstT t = translate $ \ c (a,z) -> fmap (\b -> (b,z)) (apply t c a)

------------------------------------------------------------------------------------------

instance Functor m => Functor (Translate c m a) where
  
-- fmap :: (b -> d) -> Translate c m a b -> Translate c m a d  
   fmap f t = translate (\ c -> fmap f . apply t c)

instance Pointed m => Pointed (Translate c m a) where
-- point :: b -> Translate c m a b
   point = constT

instance Applicative m => Applicative (Translate c m a) where
  
-- pure :: b -> Translate c m a b  
   pure b = translate (\ _ _ -> pure b)
   
-- (<*>) :: Translate c m a (b -> d) -> Translate c m a b -> Translate c m a d   
   tf <*> tb = translate (\ c a -> apply tf c a <*> apply tb c a) 

instance Alternative m => Alternative (Translate c m a) where

-- empty :: Translate c m a b  
   empty = translate (\ _ _ -> empty)

-- (<|>) :: Translate c m a b -> Translate c m a b -> Translate c m a b
   (<|>) = (<+)

instance Monad m => Monad (Translate c m a) where 
  
-- return :: b -> Translate c m a b
   return b = translate (\ _ _ -> return b)
   
-- (>>=) :: Translate c m a b -> (b -> Translate c m a d) -> Translate c m a d     
   tb >>= f = translate $ \ c a -> do b <- apply tb c a 
                                      apply (f b) c a
                                     
-- fail :: String -> Translate c m a b
   fail msg = translate $ \ _ _ -> fail msg

instance (Pointed m, Monad m) => Category (Translate c m) where

--  id :: Translate c m a a
    id = idR

--  (.) :: Translate c m b d -> Translate c m a b -> Translate c m a d
    t2 . t1 = t1 >-> t2
      
instance (Functor m, Pointed m, Monad m) => Arrow (Translate c m) where
  
-- arr :: (a -> b) -> Translate c m a b  
   arr = liftT

-- first :: (a -> b) -> Translate c m (a,z) (b,z)
   first = firstT
   
------------------------------------------------------------------------------------------

-- | 'concatT' turns a list of 'Translate's that return a common 'Monoid'al result
-- into a single 'Translate' that performs them all in sequence and combines their
-- results with 'mconcat'
concatT :: (Applicative m , Monoid b) => [Translate c m a b] -> Translate c m a b
concatT = liftA mconcat . sequenceA

-- | 'emptyT' is an unfailing 'Translate' that always returns 'mempty'
emptyT :: (Pointed m, Monoid b) => Translate c m a b
emptyT = constT mempty

------------------------------------------------------------------------------------------

-- | look at the argument for the translation before choosing which 'Translate' to perform.
readerT :: (a -> Translate c m a b) -> Translate c m a b
readerT f = translate $ \ c a -> apply (f a) c a

-- | look at the argument to a 'Rewrite', and choose to be either a failure or trivial success.
acceptR :: Alternative m => (a -> Bool) -> Rewrite c m a
acceptR p = rewrite $ \ _ a -> if p a then pure a else empty

-- | catch a failing 'Rewrite', making it into an identity.
tryR :: (Pointed m, Alternative m) => Rewrite c m a -> Rewrite c m a
tryR s = s <+ idR

-- | repeat a rewrite until it fails, then return the result before the failure.
repeatR :: (Pointed m, Alternative m, Monad m) => Rewrite c m a -> Rewrite c m a
repeatR s = tryR (s >-> repeatR s)

-- | Guarded translate.
(?) ::  Alternative m => Bool -> Translate c m a b -> Translate c m a b
False ? _  = failT
True  ? t  = t

------------------------------------------------------------------------------------------

-- | Equivalent to (***), but with less class constraints
tuple2R :: Applicative m => Rewrite c m a -> Rewrite c m b -> Rewrite c m (a, b)
tuple2R r1 r2 = rewrite $ \ c (a,b) -> liftA2 (,) (apply r1 c a) (apply r2 c b)
    
listR :: Applicative m => Rewrite c m a -> Rewrite c m [a]
listR r = rewrite $ \ c -> sequenceA . map (apply r c)

maybeR :: Applicative m => Rewrite c m a -> Rewrite c m (Maybe a)
maybeR r = rewrite $ \ c -> maybe (pure Nothing) (liftA Just . apply r c)

tuple2T :: (Applicative m, Monoid r) => Translate c m a r -> Translate c m b r -> Translate c m (a, b) r
tuple2T t1 t2 = translate $ \ c (a,b) -> liftA2 mappend (apply t1 c a) (apply t2 c b)

listT :: (Applicative m, Monoid r) => Translate c m a r -> Translate c m [a] r
listT t = translate $ \ c -> liftA mconcat . sequenceA . map (apply t c)

maybeT :: (Applicative m, Monoid r) => Translate c m a r -> Translate c m (Maybe a) r
maybeT t = translate $ \ c -> maybe (pure mempty) (apply t c)

------------------------------------------------------------------------------------------

-- | Translate a 'Just' a into an a, or a 'Nothing' into a failure 
fromJustT :: Alternative m => Translate c m (Maybe a) a
fromJustT = translate $ \ _ -> maybe empty pure

------------------------------------------------------------------------------------------
