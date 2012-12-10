-- |
-- Module: Language.KURE.Translate
-- Copyright: (c) 2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module defines the main KURE types: 'Translate' and 'Rewrite'.
-- 'Rewrite' is just a special case of 'Translate', and so any function that operates on 'Translate' is also
-- applicable to 'Rewrite'.
--
-- 'Translate' is an instance of the 'Monad' and 'Arrow' type-class families, and consequently
-- many of the desirable combinators over 'Translate' and 'Rewrite' are special cases
-- of existing monadic or arrow combinators.
-- "Language.KURE.Combinators" provides some additional combinators that aren't in the standard libraries.

module Language.KURE.Translate
       (-- * Translations and Rewrites
          Translate
        , Rewrite
        , apply
        , translate
        , rewrite
        , contextfreeT
        , contextonlyT
        , constT
) where

import Prelude hiding (id, (.))

import Control.Applicative
import Control.Monad
import Control.Category
import Control.Arrow

import Data.Monoid

import Language.KURE.Catch

------------------------------------------------------------------------------------------

-- | An abstract representation of a transformation from a value of type @a@ in a context @c@ to a monadic value of type @m b@.
--   The 'Translate' type is the basis of the entire KURE library.
newtype Translate c m a b = Translate { -- | Apply a 'Translate' to a value and its context.
                                        apply :: c -> a -> m b}

-- | The primitive  way of building a 'Translate'.
translate :: (c -> a -> m b) -> Translate c m a b
translate = Translate
{-# INLINE translate #-}

-- | A 'Translate' that shares the same source and target type.
type Rewrite c m a = Translate c m a a

-- | The primitive way of building a 'Rewrite'.
rewrite :: (c -> a -> m a) -> Rewrite c m a
rewrite = translate
{-# INLINE rewrite #-}

------------------------------------------------------------------------------------------

-- | Build a 'Translate' that doesn't depend on the context.
contextfreeT :: (a -> m b) -> Translate c m a b
contextfreeT f = translate (\ _ -> f)
{-# INLINE contextfreeT #-}

-- | Build a 'Translate' that doesn't depend on the value.
contextonlyT :: (c -> m b) -> Translate c m a b
contextonlyT f = translate (\ c _ -> f c)
{-# INLINE contextonlyT #-}

-- | Build a constant 'Translate' from a monadic computation.
constT :: m b -> Translate c m a b
constT = contextfreeT . const
{-# INLINE constT #-}

------------------------------------------------------------------------------------------

-- | Lifting through a Reader transformer, where (c,a) is the read-only environment.
instance Functor m => Functor (Translate c m a) where

-- fmap :: (b -> d) -> Translate c m a b -> Translate c m a d
   fmap f t = translate (\ c -> fmap f . apply t c)
   {-# INLINE fmap #-}

-- | Lifting through a Reader transformer, where (c,a) is the read-only environment.
instance Applicative m => Applicative (Translate c m a) where

-- pure :: b -> Translate c m a b
   pure = constT . pure
   {-# INLINE pure #-}

-- (<*>) :: Translate c m a (b -> d) -> Translate c m a b -> Translate c m a d
   tf <*> tb = translate (\ c a -> apply tf c a <*> apply tb c a)
   {-# INLINE (<*>) #-}

-- | Lifting through a Reader transformer, where (c,a) is the read-only environment.
instance Alternative m => Alternative (Translate c m a) where

-- empty :: Translate c m a b
   empty = constT empty
   {-# INLINE empty #-}

-- (<|>) :: Translate c m a b -> Translate c m a b -> Translate c m a b
   t1 <|> t2 = translate (\ c a -> apply t1 c a <|> apply t2 c a)
   {-# INLINE (<|>) #-}

-- | Lifting through a Reader transformer, where (c,a) is the read-only environment.
instance Monad m => Monad (Translate c m a) where

-- return :: b -> Translate c m a b
   return = constT . return
   {-# INLINE return #-}

-- (>>=) :: Translate c m a b -> (b -> Translate c m a d) -> Translate c m a d
   t >>= f = translate $ \ c a -> do b <- apply t c a
                                     apply (f b) c a
   {-# INLINE (>>=) #-}

-- fail :: String -> Translate c m a b
   fail = constT . fail
   {-# INLINE fail #-}

-- | Lifting through a Reader transformer, where (c,a) is the read-only environment.
instance MonadCatch m => MonadCatch (Translate c m a) where

-- catchM :: Translate c m a b -> (String -> Translate c m a b) -> Translate c m a b
   catchM t1 t2 = translate $ \ c a -> apply t1 c a `catchM` \ msg -> apply (t2 msg) c a
   {-# INLINE catchM #-}

-- | Lifting through a Reader transformer, where (c,a) is the read-only environment.
instance MonadPlus m => MonadPlus (Translate c m a) where

-- mzero :: Translate c m a b
   mzero = constT mzero
   {-# INLINE mzero #-}

-- mplus :: Translate c m a b -> Translate c m a b -> Translate c m a b
   mplus t1 t2 = translate $ \ c a -> apply t1 c a `mplus` apply t2 c a
   {-# INLINE mplus #-}

------------------------------------------------------------------------------------------

-- | The 'Kleisli' 'Category' induced by @m@, lifting through a Reader transformer, where @c@ is the read-only environment.
instance Monad m => Category (Translate c m) where

-- id :: Translate c m a a
   id = contextfreeT return
   {-# INLINE id #-}

-- (.) :: Translate c m b d -> Translate c m a b -> Translate c m a d
   t2 . t1 = translate (\ c -> apply t1 c >=> apply t2 c)
   {-# INLINE (.) #-}

-- | The 'Kleisli' 'Category' induced by @m@, lifting through a Reader transformer, where @c@ is the read-only environment.
instance MonadCatch m => BiCatch (Translate c m) where

-- failT :: String -> Translate c m a b
   failT = fail
   {-# INLINE failT #-}

-- catchT :: Translate c m a b -> (String -> Translate c m a b) -> Translate c m a b
   catchT = catchM
   {-# INLINE catchT #-}

-- | The 'Kleisli' 'Arrow' induced by @m@, lifting through a Reader transformer, where @c@ is the read-only environment.
instance Monad m => Arrow (Translate c m) where

-- arr :: (a -> b) -> Translate c m a b
   arr f = contextfreeT (return . f)
   {-# INLINE arr #-}

-- first :: Translate c m a b -> Translate c m (a,z) (b,z)
   first t = translate $ \ c (a,z) -> liftM (\ b -> (b,z)) (apply t c a)
   {-# INLINE first #-}

-- (***) :: Translate c m a1 b1 -> Translate c m a2 b2 -> Translate c m (a1,a2) (b1,b2)
   t1 *** t2 = translate $ \ c (a,b) -> liftM2 (,) (apply t1 c a) (apply t2 c b)
   {-# INLINE (***) #-}

-- (&&&) :: Translate c m a b1 -> Translate c m a b2 -> Translate c m a (b1,b2)
   t1 &&& t2 = translate $ \ c a -> liftM2 (,) (apply t1 c a) (apply t2 c a)
   {-# INLINE (&&&) #-}

-- | The 'Kleisli' 'Arrow' induced by @m@, lifting through a Reader transformer, where @c@ is the read-only environment.
instance MonadPlus m => ArrowZero (Translate c m) where

-- zeroArrow :: Translate c m a b
   zeroArrow = mzero
   {-# INLINE zeroArrow #-}

-- | The 'Kleisli' 'Arrow' induced by @m@, lifting through a Reader transformer, where @c@ is the read-only environment.
instance MonadPlus m => ArrowPlus (Translate c m) where

-- (<+>) :: Translate c m a b -> Translate c m a b -> Translate c m a b
   (<+>) = mplus
   {-# INLINE (<+>) #-}

-- | The 'Kleisli' 'Arrow' induced by @m@, lifting through a Reader transformer, where @c@ is the read-only environment.
instance Monad m => ArrowApply (Translate c m) where

-- app :: Translate c m (Translate c m a b, a) b
   app = translate (\ c (t,a) -> apply t c a)
   {-# INLINE app #-}

------------------------------------------------------------------------------------------

-- | Lifting through the 'Monad' and a Reader transformer, where (c,a) is the read-only environment.
instance (Monad m, Monoid b) => Monoid (Translate c m a b) where

-- mempty :: Translate c m a b
   mempty = return mempty
   {-# INLINE mempty #-}

-- mappend :: Translate c m a b -> Translate c m a b -> Translate c m a b
   mappend = liftM2 mappend
   {-# INLINE mappend #-}

------------------------------------------------------------------------------------------
