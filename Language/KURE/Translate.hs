-- |
-- Module: Language.KURE.Translate
-- Copyright: (c) 2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module defines the main KURE types: 'Translate', 'Rewrite' and 'Lens'.
-- 'Rewrite' and 'Lens' are just special cases of 'Translate', and so any function that operates on 'Translate' is also
-- applicable to 'Rewrite' and 'Lens' (although care should be taken in the 'Lens' case).
--
-- This module also contains 'Translate' instance declarations for the 'Monad' and 'Arrow' type-class families.
-- Given these instances, many of the desirable combinators over 'Translate' and 'Rewrite' are special cases
-- of existing monadic or arrow combinators.
-- "Language.KURE.Combinators" provides some additional combinators that aren't in the standard libraries.

module Language.KURE.Translate
       (  -- * Translations
          Translate(apply)
        , Rewrite
        , translate
        , rewrite
        , contextfreeT
        , constT
        , contextT
        , exposeT
        , mapT
        , sideEffectR
          -- * Lenses
        , Lens
        , applyL
        , translateL
        , lens
        , testL
        , transLens
        , pureL
        , focusR
        , focusT

) where

import Prelude hiding (id, (.))
import Control.Applicative
import Control.Monad
import Control.Category
import Control.Arrow
import Data.Monoid
import Language.KURE.Combinators

------------------------------------------------------------------------------------------

-- | 'Translate' is a translation or strategy that translates from a value in a context to a monadic value.
data Translate c m a b = Translate { -- | Apply a 'Translate' to a value and its context.
                                     apply :: c -> a -> m b}

-- | A 'Rewrite' is a 'Translate' that shares the same source and target type.
type Rewrite c m a = Translate c m a a

-- | The primitive  way of building a 'Translate'.
translate :: (c -> a -> m b) -> Translate c m a b
translate = Translate

-- | The primitive way of building a 'Rewrite'.
rewrite :: (c -> a -> m a) -> Rewrite c m a
rewrite = translate

------------------------------------------------------------------------------------------

-- | Build a 'Translate' that doesn't depend on the context.
contextfreeT :: (a -> m b) -> Translate c m a b
contextfreeT = translate . const

-- | Build a constant 'Translate' from a monadic computation.
constT :: m b -> Translate c m a b
constT = contextfreeT . const

-- | Extract the current context.
contextT :: Monad m => Translate c m a c
contextT = translate (\ c _ -> return c)

-- | Expose the current context and value.
exposeT :: Monad m => Translate c m a (c,a)
exposeT = translate (curry return)

-- | Map a 'Translate' over a list.
mapT :: Monad m => Translate c m a b -> Translate c m [a] [b]
mapT t = translate (mapM . apply t)

-- | An identity 'Rewrite' with side-effects.
sideEffectR :: Monad m => (c -> a -> m ()) -> Rewrite c m a
sideEffectR f = translate f >> id

------------------------------------------------------------------------------------------

-- | Lifting through a Reader transformer, where (c,a) is the read-only environment.
instance Functor m => Functor (Translate c m a) where

-- fmap :: (b -> d) -> Translate c m a b -> Translate c m a d
   fmap f t = translate (\ c -> fmap f . apply t c)

-- | Lifting through a Reader transformer, where (c,a) is the read-only environment.
instance Applicative m => Applicative (Translate c m a) where

-- pure :: b -> Translate c m a b
   pure = constT . pure

-- (<*>) :: Translate c m a (b -> d) -> Translate c m a b -> Translate c m a d
   tf <*> tb = translate (\ c a -> apply tf c a <*> apply tb c a)

-- | Lifting through a Reader transformer, where (c,a) is the read-only environment.
instance Alternative m => Alternative (Translate c m a) where

-- empty :: Translate c m a b
   empty = constT empty

-- (<|>) :: Translate c m a b -> Translate c m a b -> Translate c m a b
   t1 <|> t2 = translate $ \ c a -> apply t1 c a <|> apply t2 c a

-- | Lifting through a Reader transformer, where (c,a) is the read-only environment.
instance Monad m => Monad (Translate c m a) where

-- return :: b -> Translate c m a b
   return = constT . return

-- (>>=) :: Translate c m a b -> (b -> Translate c m a d) -> Translate c m a d
   t >>= f = translate $ \ c a -> do b <- apply t c a
                                     apply (f b) c a

-- fail :: String -> Translate c m a b
   fail = constT . fail

-- | Lifting through a Reader transformer, where (c,a) is the read-only environment.
instance MonadPlus m => MonadPlus (Translate c m a) where

-- mzero :: Translate c m a b
   mzero = constT mzero

-- mplus :: Translate c m a b -> Translate c m a b -> Translate c m a b
   mplus t1 t2 = translate $ \ c a -> apply t1 c a `mplus` apply t2 c a

------------------------------------------------------------------------------------------

-- | The 'Kleisli' 'Category' induced by @m@, lifting through a Reader transformer, where @c@ is the read-only environment.
instance Monad m => Category (Translate c m) where

--  id :: Translate c m a a
    id = contextfreeT return

--  (.) :: Translate c m b d -> Translate c m a b -> Translate c m a d
    t2 . t1 = translate $ \ c -> apply t1 c >=> apply t2 c

-- | The 'Kleisli' 'Category' induced by @m@, lifting through a Reader transformer, where @c@ is the read-only environment.
instance MonadPlus m => CategoryCatch (Translate c m) where

-- failR :: String -> Translate c m a b
   failR = fail

-- (<+) :: Translate c m a b -> Translate c m a b -> Translate c m a b
   (<+) = mplus


-- | The 'Kleisli' 'Arrow' induced by @m@, lifting through a Reader transformer, where @c@ is the read-only environment.
instance Monad m => Arrow (Translate c m) where

-- arr :: (a -> b) -> Translate c m a b
   arr f = contextfreeT (return . f)

-- first :: Translate c m a b -> Translate c m (a,z) (b,z)
   first t = translate $ \ c (a,z) -> liftM (\b -> (b,z)) (apply t c a)

-- (***) :: Translate c m a1 b1 -> Translate c m a2 b2 -> Translate c m (a1,a2) (b1,b2)
   t1 *** t2 = translate $ \ c (a,b) -> liftM2 (,) (apply t1 c a) (apply t2 c b)

-- (&&&) :: Translate c m a b1 -> Translate c m a b2 -> Translate c m a (b1,b2)
   t1 &&& t2 = translate $ \ c a -> liftM2 (,) (apply t1 c a) (apply t2 c a)

-- | The 'Kleisli' 'Arrow' induced by @m@, lifting through a Reader transformer, where @c@ is the read-only environment.
instance MonadPlus m => ArrowZero (Translate c m) where

-- zeroArrow :: Translate c m a b
   zeroArrow = mzero

-- | The 'Kleisli' 'Arrow' induced by @m@, lifting through a Reader transformer, where @c@ is the read-only environment.
instance MonadPlus m => ArrowPlus (Translate c m) where

-- (<+>) :: Translate c m a b -> Translate c m a b -> Translate c m a b
   (<+>) = mplus

-- | The 'Kleisli' 'Arrow' induced by @m@, lifting through a Reader transformer, where @c@ is the read-only environment.
instance Monad m => ArrowApply (Translate c m) where

-- app :: Translate c m (Translate c m a b, a) b
   app = translate $ \ c (t,a) -> apply t c a

------------------------------------------------------------------------------------------

-- | Lifting through the 'Monad' and a Reader transformer, where (c,a) is the read-only environment.
instance (Monad m, Monoid b) => Monoid (Translate c m a b) where

-- mempty :: Translate c m a b
   mempty = return mempty

-- mappend :: Translate c m a b -> Translate c m a b -> Translate c m a b
   mappend = liftM2 mappend

------------------------------------------------------------------------------------------

-- | A 'Lens' is a way to focus on a particular point in a structure.
newtype Lens c m a b = Lens {lensT :: Translate c m a ((c,b), b -> m a)}

-- | Apply a 'Lens' to a context and value.
applyL :: Lens c m a b -> c -> a -> m ((c,b), b -> m a)
applyL = apply . lensT

-- | The primitive way of building a 'Lens'.
--   If the unfocussing function is applied to the value focussed on then it should succeed,
--   and produce the same @a@ value as the argument.
translateL :: Translate c m a ((c,b), b -> m a) -> Lens c m a b
translateL = Lens

-- | Another way of building a 'Lens', defined as @(translateL . translate)@.
lens :: (c -> a -> m ((c,b), b -> m a)) -> Lens c m a b
lens = translateL . translate

-- | Checks if the focusing succeeds, and additionally whether unfocussing from an unchanged value would succeed.
testL :: MonadPlus m => Lens c m a b -> Translate c m a Bool
testL (Lens t) = testM $ do ((_,b),k) <- t
                            constT (k b)

-- | Combines a 'Translate' producing a 'Lens' into just a 'Lens'.
--   Essentially a monadic 'join'.
transLens :: Monad m => Translate c m a (Lens c m a b) -> Lens c m a b
transLens tl = translateL (tl >>= lensT)

instance Monad m => Category (Lens c m) where

-- id :: Lens c m a a
   id = lens $ \ c a -> return ((c,a), return)

-- (.) :: Lens c m b d -> Lens c m a b -> Lens c m a d
   l2 . l1 = lens $ \ ca a -> do ((cb,b),kb) <- applyL l1 ca a
                                 ((cd,d),kd) <- applyL l2 cb b
                                 return ((cd,d),kd >=> kb)


-- | '(<+)' catches a failing 'Lens'.  A 'Lens' is deemed to have failed if either it fails on the way down, or,
--   crucially, if it would fail on the way up for an unchanged value.  However, actual failure on the way up are not caught
--   (as by then it is too late).  This means that, in theory, a use of '(<+)' could cause a succeeding 'Lens' application to fail.
--   But provided |lens| is used correctly, this should never happen.

instance MonadPlus m => CategoryCatch (Lens c m) where

-- failR :: String -> Lens c m a b
   failR = translateL . fail

-- (<+) :: Lens c m a b -> Lens c m a b -> Lens c m a b
   l1 <+ l2 = translateL (condM (testL l1) (lensT l1) (lensT l2))


-- | Construct a 'Lens' from two pure functions.
pureL :: Monad m => (a -> b) -> (b -> a) -> Lens c m a b
pureL f g = lens (\ c a -> return ((c,f a), return . g))

-- | Apply a 'Rewrite' at a point specified by a 'Lens'.
focusR :: Monad m => Lens c m a b -> Rewrite c m b -> Rewrite c m a
focusR l r = rewrite $ \ c a -> do ((c',b),k) <- applyL l c a
                                   apply r c' b >>= k

-- | Apply a 'Translate' at a point specified by a 'Lens'.
focusT :: Monad m => Lens c m a b -> Translate c m b d -> Translate c m a d
focusT l t = translate $ \ c a -> do ((c',b),_) <- applyL l c a
                                     apply t c' b

------------------------------------------------------------------------------------------
