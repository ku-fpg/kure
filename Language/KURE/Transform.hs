{-# Language CPP, InstanceSigs #-}
-- |
-- Module: Language.KURE.Transform
-- Copyright: (c) 2012--2014 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module defines 'Transform' and 'Rewrite', the main KURE types.
-- 'Rewrite' is just a special case of 'Transform', and so any function that operates on 'Transform' is also
-- applicable to 'Rewrite'.
--
-- 'Transform' is an instance of the 'Monad' and 'Arrow' type-class families, and consequently
-- many of the desirable combinators over 'Transform' and 'Rewrite' are special cases
-- of existing monadic or arrow combinators.
-- "Language.KURE.Combinators" provides some additional combinators that aren't in the standard libraries.

module Language.KURE.Transform
       (-- * Transformations and Rewrites
          Transform, Translate
        , Rewrite
        , applyT, applyR, apply
        , transform, translate
        , rewrite
        , contextfreeT
        , contextonlyT
        , constT
        , effectfreeT
) where

import Prelude hiding (id, (.))

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Category
import Control.Arrow

#if __GLASGOW_HASKELL__ <= 708
import Data.Monoid
#endif

import Language.KURE.MonadCatch

------------------------------------------------------------------------------------------

-- | An abstract representation of a transformation from a value of type @a@ in a context @c@ to a monadic value of type @m b@.
--   The 'Transform' type is the basis of the entire KURE library.
newtype Transform c m a b = Transform { -- | Apply a transformation to a value and its context.
                                        applyT :: c -> a -> m b}

-- | A deprecated synonym for 'Transform'.
type Translate c m a b = Transform c m a b

-- | The primitive way of building a transformation.
transform :: (c -> a -> m b) -> Transform c m a b
transform = Transform
{-# INLINE transform #-}

-- | A deprecated synonym for 'transform'.
translate :: (c -> a -> m b) -> Translate c m a b
translate = transform
{-# INLINE translate #-}
{-# DEPRECATED translate "Please use 'transform' instead." #-}

-- | A transformation that shares the same source and target type.
type Rewrite c m a = Transform c m a a

-- | The primitive way of building a rewrite.
rewrite :: (c -> a -> m a) -> Rewrite c m a
rewrite = transform
{-# INLINE rewrite #-}

-- | Apply a rewrite to a value and its context.
applyR :: Rewrite c m a -> c -> a -> m a
applyR = applyT
{-# INLINE applyR #-}

-- | A deprecated synonym for 'applyT'.
apply :: Transform c m a b -> c -> a -> m b
apply = applyT
{-# INLINE apply #-}
{-# DEPRECATED apply "Please use 'applyT' instead." #-}

------------------------------------------------------------------------------------------

-- | Build a 'Transform' that doesn't depend on the context.
contextfreeT :: (a -> m b) -> Transform c m a b
contextfreeT f = transform (\ _ -> f)
{-# INLINE contextfreeT #-}

-- | Build a 'Transform' that doesn't depend on the value.
contextonlyT :: (c -> m b) -> Transform c m a b
contextonlyT f = transform (\ c _ -> f c)
{-# INLINE contextonlyT #-}

-- | Build a constant 'Transform' from a monadic computation.
constT :: m b -> Transform c m a b
constT = contextfreeT . const
{-# INLINE constT #-}

-- | Build a 'Transform' that doesn't perform any monadic effects.
effectfreeT :: Monad m => (c -> a -> b) -> Transform c m a b
effectfreeT f = transform ( \ c a -> return (f c a))
{-# INLINE effectfreeT #-}

------------------------------------------------------------------------------------------

-- | Lifting through a Reader transformer, where (c,a) is the read-only environment.
instance Functor m => Functor (Transform c m a) where

   fmap :: (b -> d) -> Transform c m a b -> Transform c m a d
   fmap f t = transform (\ c -> fmap f . applyT t c)
   {-# INLINE fmap #-}

-- | Lifting through a Reader transformer, where (c,a) is the read-only environment.
instance Applicative m => Applicative (Transform c m a) where

   pure :: b -> Transform c m a b
   pure = constT . pure
   {-# INLINE pure #-}

   (<*>) :: Transform c m a (b -> d) -> Transform c m a b -> Transform c m a d
   tf <*> tb = transform (\ c a -> applyT tf c a <*> applyT tb c a)
   {-# INLINE (<*>) #-}

-- | Lifting through a Reader transformer, where (c,a) is the read-only environment.
instance Alternative m => Alternative (Transform c m a) where

   empty :: Transform c m a b
   empty = constT empty
   {-# INLINE empty #-}

   (<|>) :: Transform c m a b -> Transform c m a b -> Transform c m a b
   t1 <|> t2 = transform (\ c a -> applyT t1 c a <|> applyT t2 c a)
   {-# INLINE (<|>) #-}

-- | Lifting through a Reader transformer, where (c,a) is the read-only environment.
instance Monad m => Monad (Transform c m a) where

   return :: b -> Transform c m a b
   return = constT . return
   {-# INLINE return #-}

   (>>=) :: Transform c m a b -> (b -> Transform c m a d) -> Transform c m a d
   t >>= f = transform $ \ c a -> do b <- applyT t c a
                                     applyT (f b) c a
   {-# INLINE (>>=) #-}

   fail :: String -> Transform c m a b
   fail = constT . fail
   {-# INLINE fail #-}

-- | Lifting through a Reader transformer, where (c,a) is the read-only environment.
instance MonadCatch m => MonadCatch (Transform c m a) where

   catchM :: Transform c m a b -> (String -> Transform c m a b) -> Transform c m a b
   catchM t1 t2 = transform $ \ c a -> applyT t1 c a `catchM` \ msg -> applyT (t2 msg) c a
   {-# INLINE catchM #-}

-- | Lifting through a Reader transformer, where (c,a) is the read-only environment.
instance MonadPlus m => MonadPlus (Transform c m a) where

   mzero :: Transform c m a b
   mzero = constT mzero
   {-# INLINE mzero #-}

   mplus :: Transform c m a b -> Transform c m a b -> Transform c m a b
   mplus t1 t2 = transform $ \ c a -> applyT t1 c a `mplus` applyT t2 c a
   {-# INLINE mplus #-}

-- | Lifting through a Reader transformer, where (c,a) is the read-only environment.
instance MonadIO m => MonadIO (Transform c m a) where

   liftIO :: IO b -> Transform c m a b
   liftIO = constT . liftIO
   {-# INLINE liftIO #-}

------------------------------------------------------------------------------------------

-- | The 'Kleisli' 'Category' induced by @m@, lifting through a Reader transformer, where @c@ is the read-only environment.
instance Monad m => Category (Transform c m) where

   id :: Transform c m a a
   id = contextfreeT return
   {-# INLINE id #-}

   (.) :: Transform c m b d -> Transform c m a b -> Transform c m a d
   t2 . t1 = transform (\ c -> applyT t1 c >=> applyT t2 c)
   {-# INLINE (.) #-}


-- | The 'Kleisli' 'Arrow' induced by @m@, lifting through a Reader transformer, where @c@ is the read-only environment.
instance Monad m => Arrow (Transform c m) where

   arr :: (a -> b) -> Transform c m a b
   arr f = contextfreeT (return . f)
   {-# INLINE arr #-}

   first :: Transform c m a b -> Transform c m (a,z) (b,z)
   first t = transform $ \ c (a,z) -> liftM (\ b -> (b,z)) (applyT t c a)
   {-# INLINE first #-}

   second :: Transform c m a b -> Transform c m (z,a) (z,b)
   second t = transform $ \ c (z,a) -> liftM (\ b -> (z,b)) (applyT t c a)
   {-# INLINE second #-}

   (***) :: Transform c m a1 b1 -> Transform c m a2 b2 -> Transform c m (a1,a2) (b1,b2)
   t1 *** t2 = transform $ \ c (a,b) -> liftM2 (,) (applyT t1 c a) (applyT t2 c b)
   {-# INLINE (***) #-}

   (&&&) :: Transform c m a b1 -> Transform c m a b2 -> Transform c m a (b1,b2)
   t1 &&& t2 = transform $ \ c a -> liftM2 (,) (applyT t1 c a) (applyT t2 c a)
   {-# INLINE (&&&) #-}

-- | The 'Kleisli' 'Arrow' induced by @m@, lifting through a Reader transformer, where @c@ is the read-only environment.
instance MonadPlus m => ArrowZero (Transform c m) where

   zeroArrow :: Transform c m a b
   zeroArrow = mzero
   {-# INLINE zeroArrow #-}

-- | The 'Kleisli' 'Arrow' induced by @m@, lifting through a Reader transformer, where @c@ is the read-only environment.
instance MonadPlus m => ArrowPlus (Transform c m) where

   (<+>) :: Transform c m a b -> Transform c m a b -> Transform c m a b
   (<+>) = mplus
   {-# INLINE (<+>) #-}

-- | The 'Kleisli' 'Arrow' induced by @m@, lifting through a Reader transformer, where @c@ is the read-only environment.
instance Monad m => ArrowApply (Transform c m) where

   app :: Transform c m (Transform c m a b, a) b
   app = transform (\ c (t,a) -> applyT t c a)
   {-# INLINE app #-}

------------------------------------------------------------------------------------------

-- | Lifting through the 'Monad' and a Reader transformer, where (c,a) is the read-only environment.
instance (Monad m, Monoid b) => Monoid (Transform c m a b) where

   mempty :: Transform c m a b
   mempty = return mempty
   {-# INLINE mempty #-}

   mappend :: Transform c m a b -> Transform c m a b -> Transform c m a b
   mappend = liftM2 mappend
   {-# INLINE mappend #-}

------------------------------------------------------------------------------------------
