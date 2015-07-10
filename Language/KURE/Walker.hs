{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
-- |
-- Module: Language.KURE.Walker
-- Copyright: (c) 2012--2014 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides combinators that traverse a tree.
--
-- Note that all traversals take place on the node, its children, or its descendents.
-- Deliberately, there is no mechanism for \"ascending\" the tree.

module Language.KURE.Walker
        (
        -- * Shallow Traversals

        -- ** Tree Walkers
          Walker(..)
        -- ** Child Transformations
        , childR
        , childT

        -- * Deep Traversals

        -- ** Traversals for Rewrites
        , alltdR
        , allbuR
        , allduR
        , anytdR
        , anybuR
        , anyduR
        , onetdR
        , onebuR
        , prunetdR
        , innermostR
        , allLargestR
        , anyLargestR
        , oneLargestR

        -- ** Traversals for Transformations
        , foldtdT
        , foldbuT
        , onetdT
        , onebuT
        , prunetdT
        , crushtdT
        , crushbuT
        , collectT
        , collectPruneT
        , allLargestT
        , oneLargestT

        -- * Utilitity Transformations
        , childrenT
        , summandIsTypeT

        -- * Paths
        -- ** Building Lenses from Paths
        , pathL
        , localPathL
        , exhaustPathL
        , repeatPathL
        -- ** Applying transformations at the end of Paths
        , pathR
        , pathT
        , localPathR
        , localPathT
        -- ** Testing Paths
        , testPathT
) where

import Prelude hiding (id)

import Data.Maybe (isJust)
import Data.Monoid
import Data.DList (singleton, toList)
#if __GLASGOW_HASKELL__ >= 708
import Data.Typeable
#endif

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif
import Control.Arrow
import Control.Category hiding ((.))
import Control.Monad

import Language.KURE.MonadCatch
import Language.KURE.Transform
import Language.KURE.Lens
import Language.KURE.Injection
import Language.KURE.Combinators
import Language.KURE.Path

-------------------------------------------------------------------------------

-- | 'Walker' captures the ability to walk over a tree containing nodes of type @u@,
--   using a specific context @c@.
--
--   Minimal complete definition: 'allR'.
--
--   Default definitions are provided for 'anyR', 'oneR', 'allT', 'oneT', and 'childL',
--   but they may be overridden for efficiency.

class Walker c u where

  -- | Apply a rewrite to all immediate children, succeeding if they all succeed.
  allR :: MonadCatch m => Rewrite c m u -> Rewrite c m u

  -- | Apply a transformation to all immediate children, succeeding if they all succeed.
  --   The results are combined in a 'Monoid'.
  allT :: (MonadCatch m, Monoid b) => Transform c m u b -> Transform c m u b
  allT = unwrapAllT . allR . wrapAllT
  {-# INLINE allT #-}

  -- | Apply a transformation to the first immediate child for which it can succeed.
  oneT :: MonadCatch m => Transform c m u b -> Transform c m u b
  oneT = unwrapOneT . allR . wrapOneT
  {-# INLINE oneT #-}

  -- | Apply a rewrite to all immediate children, suceeding if any succeed.
  anyR :: MonadCatch m => Rewrite c m u -> Rewrite c m u
  anyR = unwrapAnyR . allR . wrapAnyR
  {-# INLINE anyR #-}

  -- | Apply a rewrite to the first immediate child for which it can succeed.
  oneR :: MonadCatch m => Rewrite c m u -> Rewrite c m u
  oneR = unwrapOneR . allR . wrapOneR
  {-# INLINE oneR #-}

  -- | Construct a 'Lens' to the n-th child node.
  childL :: (ReadPath c crumb, Eq crumb, MonadCatch m) => crumb -> Lens c m u u
  childL = childL_default
  {-# INLINE childL #-}

#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable Walker
#endif

------------------------------------------------------------------------------------------

-- | List the children of the current node.
childrenT :: (ReadPath c crumb, Walker c u, MonadCatch m) => Transform c m u [crumb]
childrenT = allT (lastCrumbT >>^ return)
{-# INLINE childrenT #-}

-------------------------------------------------------------------------------

-- | Apply a transformation to a specified child.
childT :: (ReadPath c crumb, Eq crumb, Walker c u, MonadCatch m) => crumb -> Transform c m u b -> Transform c m u b
childT n = focusT (childL n)
{-# INLINE childT #-}

-- | Apply a rewrite to a specified child.
childR :: (ReadPath c crumb, Eq crumb, Walker c u, MonadCatch m) => crumb -> Rewrite c m u -> Rewrite c m u
childR n = focusR (childL n)
{-# INLINE childR #-}

-------------------------------------------------------------------------------

-- | Fold a tree in a top-down manner, using a single 'Transform' for each node.
foldtdT :: (Walker c u, MonadCatch m, Monoid b) => Transform c m u b -> Transform c m u b
foldtdT t = prefixFailMsg "foldtdT failed: " $
            let go = t <> allT go
             in go
{-# INLINE foldtdT #-}

-- | Fold a tree in a bottom-up manner, using a single 'Transform' for each node.
foldbuT :: (Walker c u, MonadCatch m, Monoid b) => Transform c m u b -> Transform c m u b
foldbuT t = prefixFailMsg "foldbuT failed: " $
            let go = allT go <> t
             in go
{-# INLINE foldbuT #-}

-- | Apply a transformation to the first node for which it can succeed, in a top-down traversal.
onetdT :: (Walker c u, MonadCatch m) => Transform c m u b -> Transform c m u b
onetdT t = setFailMsg "onetdT failed" $
           let go = t <+ oneT go
            in go
{-# INLINE onetdT #-}

-- | Apply a transformation to the first node for which it can succeed, in a bottom-up traversal.
onebuT :: (Walker c u, MonadCatch m) => Transform c m u b -> Transform c m u b
onebuT t = setFailMsg "onebuT failed" $
           let go = oneT go <+ t
            in go
{-# INLINE onebuT #-}

-- | Attempt to apply a 'Transform' in a top-down manner, pruning at successes.
prunetdT :: (Walker c u, MonadCatch m, Monoid b) => Transform c m u b -> Transform c m u b
prunetdT t = setFailMsg "prunetdT failed" $
             let go = t <+ allT go
              in go
{-# INLINE prunetdT #-}

-- | An always successful top-down fold, replacing failures with 'mempty'.
crushtdT :: (Walker c u, MonadCatch m, Monoid b) => Transform c m u b -> Transform c m u b
crushtdT t = foldtdT (mtryM t)
{-# INLINE crushtdT #-}

-- | An always successful bottom-up fold, replacing failures with 'mempty'.
crushbuT :: (Walker c u, MonadCatch m, Monoid b) => Transform c m u b -> Transform c m u b
crushbuT t = foldbuT (mtryM t)
{-# INLINE crushbuT #-}

-- | An always successful traversal that collects the results of all successful applications of a 'Transform' in a list.
collectT :: (Walker c u, MonadCatch m) => Transform c m u b -> Transform c m u [b]
collectT t = crushtdT (t >>^ singleton) >>^ toList
{-# INLINE collectT #-}

-- | Like 'collectT', but does not traverse below successes.
collectPruneT :: (Walker c u, MonadCatch m) => Transform c m u b -> Transform c m u [b]
collectPruneT t = prunetdT (t >>^ singleton) >>^ toList
{-# INLINE collectPruneT #-}

-------------------------------------------------------------------------------

-- | Apply a rewrite in a top-down manner, succeeding if they all succeed.
alltdR :: (Walker c u, MonadCatch m) => Rewrite c m u -> Rewrite c m u
alltdR r = prefixFailMsg "alltdR failed: " $
           let go = r >>> allR go
            in go
{-# INLINE alltdR #-}

-- | Apply a rewrite in a bottom-up manner, succeeding if they all succeed.
allbuR :: (Walker c u, MonadCatch m) => Rewrite c m u -> Rewrite c m u
allbuR r = prefixFailMsg "allbuR failed: " $
           let go = allR go >>> r
            in go
{-# INLINE allbuR #-}

-- | Apply a rewrite twice, in a top-down and bottom-up way, using one single tree traversal,
--   succeeding if they all succeed.
allduR :: (Walker c u, MonadCatch m) => Rewrite c m u -> Rewrite c m u
allduR r = prefixFailMsg "allduR failed: " $
           let go = r >>> allR go >>> r
            in go
{-# INLINE allduR #-}

-- | Apply a rewrite in a top-down manner, succeeding if any succeed.
anytdR :: (Walker c u, MonadCatch m) => Rewrite c m u -> Rewrite c m u
anytdR r = setFailMsg "anytdR failed" $
           let go = r >+> anyR go
            in go
{-# INLINE anytdR #-}

-- | Apply a rewrite in a bottom-up manner, succeeding if any succeed.
anybuR :: (Walker c u, MonadCatch m) => Rewrite c m u -> Rewrite c m u
anybuR r = setFailMsg "anybuR failed" $
           let go = anyR go >+> r
            in go
{-# INLINE anybuR #-}

-- | Apply a rewrite twice, in a top-down and bottom-up way, using one single tree traversal,
--   succeeding if any succeed.
anyduR :: (Walker c u, MonadCatch m) => Rewrite c m u -> Rewrite c m u
anyduR r = setFailMsg "anyduR failed" $
           let go = r >+> anyR go >+> r
            in go
{-# INLINE anyduR #-}

-- | Apply a rewrite to the first node for which it can succeed, in a top-down traversal.
onetdR :: (Walker c u, MonadCatch m) => Rewrite c m u -> Rewrite c m u
onetdR r = setFailMsg "onetdR failed" $
           let go = r <+ oneR go
            in go
{-# INLINE onetdR #-}

-- | Apply a rewrite to the first node for which it can succeed, in a bottom-up traversal.
onebuR :: (Walker c u, MonadCatch m) => Rewrite c m u -> Rewrite c m u
onebuR r = setFailMsg "onebuR failed" $
           let go = oneR go <+ r
            in go
{-# INLINE onebuR #-}

-- | Attempt to apply a 'Rewrite' in a top-down manner, pruning at successful rewrites.
prunetdR :: (Walker c u, MonadCatch m) => Rewrite c m u -> Rewrite c m u
prunetdR r = setFailMsg "prunetdR failed" $
             let go = r <+ anyR go
              in go
{-# INLINE prunetdR #-}

-- | A fixed-point traveral, starting with the innermost term.
innermostR :: (Walker c u, MonadCatch m) => Rewrite c m u -> Rewrite c m u
innermostR r = setFailMsg "innermostR failed" $
               let go = anybuR (r >>> tryR go)
                in go
{-# INLINE innermostR #-}

-------------------------------------------------------------------------------

tryL :: MonadCatch m => Lens c m u u -> Lens c m u u
tryL l = l `catchL` (\ _ -> id)
{-# INLINE tryL #-}

-- | Construct a 'Lens' by following a 'Path'.
pathL :: (ReadPath c crumb, Eq crumb, Walker c u, MonadCatch m) => Path crumb -> Lens c m u u
pathL = serialise . map childL
{-# INLINE pathL #-}

-- | Build a 'Lens' from the root to a point specified by a 'LocalPath'.
localPathL :: (ReadPath c crumb, Eq crumb, Walker c u, MonadCatch m) => LocalPath crumb -> Lens c m u u
localPathL = pathL . snocPathToPath
{-# INLINE localPathL #-}

-- | Construct a 'Lens' that points to the last node at which the 'Path' can be followed.
exhaustPathL :: (ReadPath c crumb, Eq crumb, Walker c u, MonadCatch m) => Path crumb -> Lens c m u u
exhaustPathL = foldr (\ n l -> tryL (childL n >>> l)) id
{-# INLINE exhaustPathL #-}

-- | Repeat as many iterations of the 'Path' as possible.
repeatPathL :: (ReadPath c crumb, Eq crumb, Walker c u, MonadCatch m) => Path crumb -> Lens c m u u
repeatPathL p = let go = tryL (pathL p >>> go)
                 in go
{-# INLINE repeatPathL #-}

-------------------------------------------------------------------------------

-- | Apply a rewrite at a point specified by a 'Path'.
pathR :: (ReadPath c crumb, Eq crumb, Walker c u, MonadCatch m) => Path crumb -> Rewrite c m u -> Rewrite c m u
pathR = focusR . pathL
{-# INLINE pathR #-}

-- | Apply a transformation at a point specified by a 'Path'.
pathT :: (ReadPath c crumb, Eq crumb, Walker c u, MonadCatch m) => Path crumb -> Transform c m u b -> Transform c m u b
pathT = focusT . pathL
{-# INLINE pathT #-}

-- | Apply a rewrite at a point specified by a 'LocalPath'.
localPathR :: (ReadPath c crumb, Eq crumb, Walker c u, MonadCatch m) => LocalPath crumb -> Rewrite c m u -> Rewrite c m u
localPathR = focusR . localPathL
{-# INLINE localPathR #-}

-- | Apply a transformation at a point specified by a 'LocalPath'.
localPathT :: (ReadPath c crumb, Eq crumb, Walker c u, MonadCatch m) => LocalPath crumb -> Transform c m u b -> Transform c m u b
localPathT = focusT . localPathL
{-# INLINE localPathT #-}

-------------------------------------------------------------------------------

-- | Check if it is possible to construct a 'Lens' along this path from the current node.
testPathT :: (ReadPath c crumb, Eq crumb, Walker c u, MonadCatch m) => Path crumb -> Transform c m u Bool
testPathT = testLensT . pathL
{-# INLINE testPathT #-}

-------------------------------------------------------------------------------

-- | Apply a rewrite to the largest node(s) that satisfy the predicate, requiring all to succeed.
allLargestR :: (Walker c u, MonadCatch m) => Transform c m u Bool -> Rewrite c m u -> Rewrite c m u
allLargestR p r = prefixFailMsg "allLargestR failed: " $
                  let go = ifM p r (allR go)
                   in go
{-# INLINE allLargestR #-}

-- | Apply a rewrite to the largest node(s) that satisfy the predicate, succeeding if any succeed.
anyLargestR :: (Walker c u, MonadCatch m) => Transform c m u Bool -> Rewrite c m u -> Rewrite c m u
anyLargestR p r = setFailMsg "anyLargestR failed" $
                  let go = ifM p r (anyR go)
                   in go
{-# INLINE anyLargestR #-}

-- | Apply a rewrite to the first node for which it can succeed among the largest node(s) that satisfy the predicate.
oneLargestR :: (Walker c u, MonadCatch m) => Transform c m u Bool -> Rewrite c m u -> Rewrite c m u
oneLargestR p r = setFailMsg "oneLargestR failed" $
                  let go = ifM p r (oneR go)
                   in go
{-# INLINE oneLargestR #-}

-- | Apply a transformation to the largest node(s) that satisfy the predicate, combining the results in a monoid.
allLargestT :: (Walker c u, MonadCatch m, Monoid b) => Transform c m u Bool -> Transform c m u b -> Transform c m u b
allLargestT p t = prefixFailMsg "allLargestT failed: " $
                  let go = ifM p t (allT go)
                   in go
{-# INLINE allLargestT #-}

-- | Apply a transformation to the first node for which it can succeed among the largest node(s) that satisfy the predicate.
oneLargestT :: (Walker c u, MonadCatch m) => Transform c m u Bool -> Transform c m u b -> Transform c m u b
oneLargestT p t = setFailMsg "oneLargestT failed" $
                  let go = ifM p t (oneT go)
                   in go
{-# INLINE oneLargestT #-}

-- | Test if the type of the current node summand matches the type of the argument.
--   Note that the argument /value/ is never inspected, it is merely a proxy for a type argument.
summandIsTypeT :: forall c m a u. (MonadCatch m, Injection a u) => a -> Transform c m u Bool
summandIsTypeT _ = arr (isJust . (project :: (u -> Maybe a)))
{-# INLINE summandIsTypeT #-}

-------------------------------------------------------------------------------

data P a b = P a b

pSnd :: P a b -> b
pSnd (P _ b) = b
{-# INLINE pSnd #-}

checkSuccessPMaybe :: Monad m => String -> m (Maybe a) -> m a
checkSuccessPMaybe msg ma = ma >>= projectWithFailMsgM msg
{-# INLINE checkSuccessPMaybe #-}

-------------------------------------------------------------------------------

-- These are used for defining 'allT' in terms of 'allR'.
-- However, they are unlikely to be of use to the KURE user.

newtype AllT w m a = AllT (m (P a w))

unAllT :: AllT w m a -> m (P a w)
unAllT (AllT mw) = mw
{-# INLINE unAllT #-}

instance (Monoid w, Monad m) => Functor (AllT w m) where
   fmap :: (a -> b) -> AllT w m a -> AllT w m b
   fmap = liftM
   {-# INLINE fmap #-}

instance (Monoid w, Monad m) => Applicative (AllT w m) where
   pure :: a -> AllT w m a
   pure = return
   {-# INLINE pure #-}

   (<*>) :: AllT w m (a -> b) -> AllT w m a -> AllT w m b
   (<*>) = ap
   {-# INLINE (<*>) #-}

instance (Monoid w, Monad m) => Monad (AllT w m) where
   return :: a -> AllT w m a
   return a = AllT $ return (P a mempty)
   {-# INLINE return #-}

   fail :: String -> AllT w m a
   fail = AllT . fail
   {-# INLINE fail #-}

   (>>=) :: AllT w m a -> (a -> AllT w m d) -> AllT w m d
   ma >>= f = AllT $ do P a w1 <- unAllT ma
                        P d w2 <- unAllT (f a)
                        return (P d (w1 <> w2))
   {-# INLINE (>>=) #-}

instance (Monoid w, MonadCatch m) => MonadCatch (AllT w m) where
   catchM :: AllT w m a -> (String -> AllT w m a) -> AllT w m a
   catchM (AllT ma) f = AllT $ ma `catchM` (unAllT . f)
   {-# INLINE catchM #-}


-- | Wrap a 'Transform' using the 'AllT' monad transformer.
wrapAllT :: Monad m => Transform c m u b -> Rewrite c (AllT b m) u
wrapAllT t = readerT $ \ a -> resultT (AllT . liftM (P a)) t
{-# INLINE wrapAllT #-}

-- | Unwrap a 'Transform' from the 'AllT' monad transformer.
unwrapAllT :: MonadCatch m => Rewrite c (AllT b m) u -> Transform c m u b
unwrapAllT = prefixFailMsg "allT failed:" . resultT (liftM pSnd . unAllT)
{-# INLINE unwrapAllT #-}

-------------------------------------------------------------------------------

-- We could probably build this on top of OneR or AllT

-- These are used for defining 'oneT' in terms of 'allR'.
-- However, they are unlikely to be of use to the KURE user.

newtype OneT w m a = OneT (Maybe w -> m (P a (Maybe w)))

unOneT :: OneT w m a -> Maybe w -> m (P a (Maybe w))
unOneT (OneT f) = f
{-# INLINE unOneT #-}

instance Monad m => Functor (OneT w m) where
   fmap :: (a -> b) -> OneT w m a -> OneT w m b
   fmap = liftM
   {-# INLINE fmap #-}

instance Monad m => Applicative (OneT w m) where
   pure :: a -> OneT w m a
   pure = return
   {-# INLINE pure #-}

   (<*>) :: OneT w m (a -> b) -> OneT w m a -> OneT w m b
   (<*>) = ap
   {-# INLINE (<*>) #-}

instance Monad m => Monad (OneT w m) where
   return :: a -> OneT w m a
   return a = OneT $ \ mw -> return (P a mw)
   {-# INLINE return #-}

   fail :: String -> OneT w m a
   fail msg = OneT (\ _ -> fail msg)
   {-# INLINE fail #-}

   (>>=) :: OneT w m a -> (a -> OneT w m d) -> OneT w m d
   ma >>= f = OneT $ do \ mw1 -> do P a mw2 <- unOneT ma mw1
                                    unOneT (f a) mw2
   {-# INLINE (>>=) #-}

instance MonadCatch m => MonadCatch (OneT w m) where
   catchM :: OneT w m a -> (String -> OneT w m a) -> OneT w m a
   catchM (OneT g) f = OneT $ \ mw -> g mw `catchM` (($ mw) . unOneT . f)
   {-# INLINE catchM #-}


-- | Wrap a 'Transform' using the 'OneT' monad transformer.
wrapOneT :: MonadCatch m => Transform c m u b -> Rewrite c (OneT b m) u
wrapOneT t = rewrite $ \ c a -> OneT $ \ mw -> case mw of
                                                 Just w  -> return (P a (Just w))
                                                 Nothing -> ((P a . Just) `liftM` applyT t c a) <+ return (P a mw)
{-# INLINE wrapOneT #-}

-- | Unwrap a 'Transform' from the 'OneT' monad transformer.
unwrapOneT :: Monad m => Rewrite c (OneT b m) u -> Transform c m u b
unwrapOneT = resultT (checkSuccessPMaybe "oneT failed" . liftM pSnd . ($ Nothing) . unOneT)
{-# INLINE unwrapOneT #-}

-------------------------------------------------------------------------------

-- If allR just used Monad (rather than MonadCatch), this (and other things) would be simpler.
-- And currently, the only use of MonadCatch is that it allows the error message to be modified.

-- Failure should not occur, so it doesn't really matter where the KureM monad sits in the GetChild stack.
-- I've arbitrarily made it a local failure.

data GetChild c u a = GetChild (KureM a) (Maybe (c,u))

getChildSecond :: (Maybe (c,u) -> Maybe (c,u)) -> GetChild c u a -> GetChild c u a
getChildSecond f (GetChild ka mcu) = GetChild ka (f mcu)
{-# INLINE getChildSecond #-}

instance Functor (GetChild c u) where
   fmap :: (a -> b) -> GetChild c u a -> GetChild c u b
   fmap = liftM
   {-# INLINE fmap #-}

instance Applicative (GetChild c u) where
   pure :: a -> GetChild c u a
   pure = return
   {-# INLINE pure #-}

   (<*>) :: GetChild c u (a -> b) -> GetChild c u a -> GetChild c u b
   (<*>) = ap
   {-# INLINE (<*>) #-}

instance Monad (GetChild c u) where
   return :: a -> GetChild c u a
   return a = GetChild (return a) Nothing
   {-# INLINE return #-}

   fail :: String -> GetChild c u a
   fail msg = GetChild (fail msg) Nothing
   {-# INLINE fail #-}

   (>>=) :: GetChild c u a -> (a -> GetChild c u b) -> GetChild c u b
   (GetChild kma mcu) >>= k = runKureM (\ a   -> getChildSecond (mplus mcu) (k a))
                                       (\ msg -> GetChild (fail msg) mcu)
                                       kma
   {-# INLINE (>>=) #-}

instance MonadCatch (GetChild c u) where
   catchM :: GetChild c u a -> (String -> GetChild c u a) -> GetChild c u a
   uc@(GetChild kma mcu) `catchM` k = runKureM (\ _   -> uc)
                                               (\ msg -> getChildSecond (mplus mcu) (k msg))
                                               kma
   {-# INLINE catchM #-}


wrapGetChild :: (ReadPath c crumb, Eq crumb) => crumb -> Rewrite c (GetChild c g) g
wrapGetChild cr = do cr' <- lastCrumbT
                     rewrite $ \ c a -> GetChild (return a) (if cr == cr' then Just (c, a) else Nothing)
{-# INLINE wrapGetChild #-}

unwrapGetChild :: Rewrite c (GetChild c u) u -> Transform c Maybe u (c,u)
unwrapGetChild = resultT (\ (GetChild _ mcu) -> mcu)
{-# INLINE unwrapGetChild #-}

getChild :: (ReadPath c crumb, Eq crumb, Walker c u) => crumb -> Transform c Maybe u (c, u)
getChild = unwrapGetChild . allR . wrapGetChild
{-# INLINE getChild #-}

-------------------------------------------------------------------------------

type SetChild = KureM

wrapSetChild :: (ReadPath c crumb, Eq crumb) => crumb -> u -> Rewrite c SetChild u
wrapSetChild cr u = do cr' <- lastCrumbT
                       if cr == cr' then return u else idR
{-# INLINE wrapSetChild #-}

unwrapSetChild :: Monad m => Rewrite c SetChild u -> Rewrite c m u
unwrapSetChild = resultT liftKureM
{-# INLINE unwrapSetChild #-}

setChild :: (ReadPath c crumb, Eq crumb, Walker c u, Monad m) => crumb -> u -> Rewrite c m u
setChild cr = unwrapSetChild . allR . wrapSetChild cr
{-# INLINE setChild #-}

-------------------------------------------------------------------------------

childL_default :: forall c crumb m u. (ReadPath c crumb, Eq crumb) => (Walker c u, MonadCatch m) => crumb -> Lens c m u u
childL_default cr = lens $ prefixFailMsg "childL failed: " $
                           do cu <- getter
                              k  <- setter
                              return (cu, k)
  where
    getter :: Transform c m u (c,u)
    getter = resultT (projectWithFailMsgM "there is no child matching the crumb.") (getChild cr)
    {-# INLINE getter #-}

    setter :: Transform c m u (u -> m u)
    setter = transform $ \ c a -> return (\ b -> applyR (setChild cr b) c a)
    {-# INLINE setter #-}
{-# INLINE childL_default #-}

-------------------------------------------------------------------------------
