{-# LANGUAGE TupleSections #-}

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
       (  -- * Translations
          Translate
        , translate 
        , apply  
        , (<+)
        , (>->)
        , contextT
        , exposeContextT
        , liftMT  
        , liftT
        , constMT
        , mconcatT
        , memptyT
        , readerT
        , whenT
        , tryT  
        , attemptT
        , testT  
          -- * Rewrites
        , Rewrite  
        , rewrite  
        , idR
        , guardR  
        , acceptR
        , tryR  
        , attemptR  
        , repeatR  
        , (>+>)
        , orR  
          -- * Prelude combinators          
        , tuple2R
        , listR
        , maybeR
        , tuple2T
        , listT
        , maybeT  
          -- * Lenses
        , Lens  
        , lens
        , idL
        , tryL  
        , composeL  
        , sequenceL
        , rewriteL  
        , translateL
) where

import Prelude hiding (id, (.))
import Data.Monoid
import Data.Maybe (isJust)
import Data.Traversable (sequenceA)
import Control.Applicative
import Control.Monad
import Control.Category
import Control.Arrow

infixl 3 <+, >->, >+>

------------------------------------------------------------------------------------------

-- | 'Translate' is a translation or strategy that translates from a value in a context to a monadic value.
data Translate c m a b = Translate {apply :: c -> a -> m b}

-- | A 'Rewrite' is a 'Translate' that shares the same source and target type.
type Rewrite c m a = Translate c m a a

-- | 'translate' is the primitive  way of building a 'Translate'.
translate :: (c -> a -> m b) -> Translate c m a b
translate = Translate

-- | 'rewrite' is the primitive way of building a 'Rewrite'.
rewrite :: (c -> a -> m a) -> Rewrite c m a
rewrite = translate

------------------------------------------------------------------------------------------

-- | identity rewrite.
idR :: Applicative m => Rewrite c m a
idR = rewrite (\ _ -> pure)
      
-- | extract the current context.
contextT :: Applicative m => Translate c m a c
contextT = translate (\ c _ -> pure c)

-- | expose the current context along with the expression.
exposeContextT :: Applicative m => Translate c m a (c,a)
exposeContextT = translate (\ c a -> pure (c,a))

-- | lift an effectful function into a 'Translate'.
liftMT :: (a -> m b) -> Translate c m a b
liftMT = translate . const

-- | lift a pure function into a 'Translate'.
liftT :: Applicative m => (a -> b) -> Translate c m a b
liftT f = liftMT (pure . f)

-- | 'constMT' lifts an effectful computation into a constant 'Translate'.
constMT :: m b -> Translate c m a b
constMT = liftMT . const

-- | like a catch, '<+' does the first 'Translate', and if it fails, then does the second 'Translate'.
(<+) :: Alternative m => Translate c m a b -> Translate c m a b -> Translate c m a b
t1 <+ t2 = translate $ \ c a -> apply t1 c a <|> apply t2 c a

-- | sequencing translates.
(>->) :: Monad m => Translate c m a b -> Translate c m b d -> Translate c m a d
t1 >-> t2 = translate $ \ c a -> apply t1 c a >>= apply t2 c

------------------------------------------------------------------------------------------

instance Functor m => Functor (Translate c m a) where
  
-- fmap :: (b -> d) -> Translate c m a b -> Translate c m a d  
   fmap f t = translate (\ c -> fmap f . apply t c)

instance Applicative m => Applicative (Translate c m a) where
  
-- pure :: b -> Translate c m a b  
   pure = constMT . pure
   
-- (<*>) :: Translate c m a (b -> d) -> Translate c m a b -> Translate c m a d   
   tf <*> tb = translate (\ c a -> apply tf c a <*> apply tb c a) 

instance Alternative m => Alternative (Translate c m a) where

-- empty :: Translate c m a b  
   empty = constMT empty

-- (<|>) :: Translate c m a b -> Translate c m a b -> Translate c m a b
   (<|>) = (<+)

instance Monad m => Monad (Translate c m a) where 
  
-- return :: b -> Translate c m a b
   return = constMT . return
   
-- (>>=) :: Translate c m a b -> (b -> Translate c m a d) -> Translate c m a d     
   tb >>= f = translate $ \ c a -> do b <- apply tb c a 
                                      apply (f b) c a
                                     
-- fail :: String -> Translate c m a b
   fail = constMT . fail

instance (Applicative m, Monad m) => Category (Translate c m) where

--  id :: Translate c m a a
    id = idR

--  (.) :: Translate c m b d -> Translate c m a b -> Translate c m a d
    t2 . t1 = t1 >-> t2
      
instance (Applicative m, Monad m) => Arrow (Translate c m) where
  
-- arr :: (a -> b) -> Translate c m a b  
   arr = liftT

-- first :: (a -> b) -> Translate c m (a,z) (b,z)
   first t = translate $ \ c (a,z) -> liftA (\b -> (b,z)) (apply t c a)
   
------------------------------------------------------------------------------------------

-- | 'concatT' turns a list of 'Translate's that return a common 'Monoid'al result
-- into a single 'Translate' that performs them all in sequence and combines their
-- results with 'mconcat'
mconcatT :: (Applicative m , Monoid b) => [Translate c m a b] -> Translate c m a b
mconcatT = liftA mconcat . sequenceA

-- | 'emptyT' is an unfailing 'Translate' that always returns 'mempty'
memptyT :: (Applicative m, Monoid b) => Translate c m a b
memptyT = pure mempty

------------------------------------------------------------------------------------------

-- | look at the argument for the translation before choosing which 'Translate' to perform.
readerT :: (a -> Translate c m a b) -> Translate c m a b
readerT f = translate $ \ c a -> apply (f a) c a

-- | guarded translate.
whenT ::  Alternative m => Bool -> Translate c m a b -> Translate c m a b
whenT False _  = empty
whenT True  t  = t

-- | guarded rewrite.
guardR :: Alternative m => Bool -> Rewrite c m a
guardR False = empty
guardR True  = idR

-- | look at the argument to a 'Rewrite', and choose to be either a failure or trivial success.
acceptR :: Alternative m => (a -> Bool) -> Rewrite c m a
acceptR p = rewrite $ \ _ a -> if p a then pure a else empty

-- | catch a failing 'Translate', making it succeed with a constant value.
tryT :: Alternative m => b -> Translate c m a b -> Translate c m a b
tryT b t = t <+ pure b

-- | catch a failing 'Rewrite', making it into an identity.
tryR :: Alternative m => Rewrite c m a -> Rewrite c m a
tryR r = r <+ idR

-- | catch a failing 'Translate', making it succeed with 'Nothing'.
attemptT :: Alternative m => Translate c m a b -> Translate c m a (Maybe b)
attemptT t = tryT Nothing (Just <$> t)

-- | catch a failing 'Rewrite', making it succeed with a Boolean flag.
--   Useful when defining @anyR@ instances.
attemptR :: Alternative m => Rewrite c m a -> Translate c m a (Bool,a)
attemptR r = fmap (True,) r <+ fmap (False,) idR

-- | determine if a 'Translate' could succeed
testT :: Alternative m => Translate c m a b -> Translate c m a Bool
testT t = isJust <$> attemptT t

-- | repeat a 'Rewrite' until it fails, then return the result before the failure.
repeatR :: (Alternative m, Monad m) => Rewrite c m a -> Rewrite c m a
repeatR r = tryR (r >-> repeatR r)

-- | attempts two 'Rewrite's in sequence, succeeding if one or both succeed.
(>+>) :: (Alternative m, Monad m) => Rewrite c m a -> Rewrite c m a -> Rewrite c m a
r1 >+> r2 = rewrite $ \ c a -> apply (attemptT r1) c a >>= maybe (apply r2 c a) (apply (tryR r2) c)
  
-- | attempt a list of 'Rewrite's in sequence, succeeding if at least one succeeds.
orR :: (Alternative m, Monad m) => [Rewrite c m a] -> Rewrite c m a
orR = foldl (>+>) empty

------------------------------------------------------------------------------------------

-- | Equivalent to (***), but with less class constraints
tuple2R :: Applicative m => Rewrite c m a -> Rewrite c m b -> Rewrite c m (a,b)
tuple2R r1 r2 = rewrite $ \ c (a,b) -> (,) <$> apply r1 c a <*> apply r2 c b
    
listR :: Applicative m => Rewrite c m a -> Rewrite c m [a]
listR r = rewrite $ \ c -> sequenceA . map (apply r c)

maybeR :: Applicative m => Rewrite c m a -> Rewrite c m (Maybe a)
maybeR r = rewrite $ \ c -> maybe (pure Nothing) (fmap Just . apply r c)

tuple2T :: (Applicative m, Monoid r) => Translate c m a r -> Translate c m b r -> Translate c m (a,b) r
tuple2T t1 t2 = translate $ \ c (a,b) -> mappend <$> apply t1 c a <*> apply t2 c b

listT :: (Applicative m, Monoid r) => Translate c m a r -> Translate c m [a] r
listT t = translate $ \ c -> fmap mconcat . sequenceA . map (apply t c)

maybeT :: (Applicative m, Monoid r) => Translate c m a r -> Translate c m (Maybe a) r
maybeT t = translate $ \ c -> maybe (pure mempty) (apply t c)

------------------------------------------------------------------------------------------

-- | A Lens is a way to focus in on a particular point in a structure
type Lens c m a b = Translate c m a ((c,b), (b -> m a))

-- | 'lens' is the primitive way of building a 'Lens'.
lens :: (c -> a -> m ((c,b), (b -> m a))) -> Lens c m a b
lens = translate

-- | identity lens.
idL :: Applicative m => Lens c m a a
idL = lens $ \ c a -> pure ((c,a), pure)

-- | catch a failing endo'Lens', making it into an identity.
tryL :: Alternative m => Lens c m a a -> Lens c m a a
tryL l = l <+ idL

-- | composition of 'Lens's.
composeL :: Monad m => Lens c m a b -> Lens c m b d -> Lens c m a d
composeL l1 l2 = lens $ \ ca a -> do ((cb,b),kb) <- apply l1 ca a
                                     ((cd,d),kd) <- apply l2 cb b
                                     return ((cd,d),kd >=> kb)

-- | sequence a list of endo'Lens's.
sequenceL :: (Applicative m, Monad m) => [Lens c m a a] -> Lens c m a a
sequenceL = foldr composeL idL

-- | apply a 'Rewrite' at a point specified by a 'Lens'.
rewriteL :: Monad m => Lens c m a b -> Rewrite c m b -> Rewrite c m a
rewriteL l r = rewrite $ \ c a -> do ((cb,b),kb) <- apply l c a
                                     apply r cb b >>= kb

-- | extract a 'Translate' from a 'Lens'
translateL :: Functor m => Lens c m a b -> Translate c m a b
translateL = fmap (snd.fst)

------------------------------------------------------------------------------------------
