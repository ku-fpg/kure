{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts #-}

-- |
-- Module: Language.KURE.Walker
-- Copyright: (c) 2012--2013 The University of Kansas
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

        -- ** Rewrite Traversals
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

        -- ** Translate Traversals
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

        -- * Utilitity Translations
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

import Control.Monad
import Control.Arrow
import Control.Category hiding ((.))

import Language.KURE.MonadCatch
import Language.KURE.Translate
import Language.KURE.Lens
import Language.KURE.Injection
import Language.KURE.Combinators
import Language.KURE.Path

-------------------------------------------------------------------------------

-- | 'Walker' captures the ability to walk over a tree containing nodes of type @g@,
--   using a specific context @c@.
--
--   Minimal complete definition: 'allR'.
--
--   Default definitions are provided for 'anyR', 'oneR', 'allT', 'oneT', and 'childL',
--   but they may be overridden for efficiency.

class Walker c g where

  -- | Apply a 'Rewrite' to all immediate children, succeeding if they all succeed.
  allR :: MonadCatch m => Rewrite c m g -> Rewrite c m g

  -- | Apply a 'Translate' to all immediate children, succeeding if they all succeed.
  --   The results are combined in a 'Monoid'.
  allT :: (MonadCatch m, Monoid b) => Translate c m g b -> Translate c m g b
  allT = unwrapAllT . allR . wrapAllT
  {-# INLINE allT #-}

  -- | Apply a 'Translate' to the first immediate child for which it can succeed.
  oneT :: MonadCatch m => Translate c m g b -> Translate c m g b
  oneT = unwrapOneT . allR . wrapOneT
  {-# INLINE oneT #-}

  -- | Apply a 'Rewrite' to all immediate children, suceeding if any succeed.
  anyR :: MonadCatch m => Rewrite c m g -> Rewrite c m g
  anyR = unwrapAnyR . allR . wrapAnyR
  {-# INLINE anyR #-}

  -- | Apply a 'Rewrite' to the first immediate child for which it can succeed.
  oneR :: MonadCatch m => Rewrite c m g -> Rewrite c m g
  oneR = unwrapOneR . allR . wrapOneR
  {-# INLINE oneR #-}

  -- | Construct a 'Lens' to the n-th child node.
  childL :: (ReadPath c crumb, Eq crumb, MonadCatch m) => crumb -> Lens c m g g
  childL = childL_default
  {-# INLINE childL #-}

------------------------------------------------------------------------------------------

-- | List the children of the current node.
childrenT :: (ReadPath c crumb, Walker c g, MonadCatch m) => Translate c m g [crumb]
childrenT = allT (lastCrumbT >>^ return)
{-# INLINE childrenT #-}

-------------------------------------------------------------------------------

-- | Apply a 'Translate' to a specified child.
childT :: (ReadPath c crumb, Eq crumb, Walker c g, MonadCatch m) => crumb -> Translate c m g b -> Translate c m g b
childT n = focusT (childL n)
{-# INLINE childT #-}

-- | Apply a 'Rewrite' to a specified child.
childR :: (ReadPath c crumb, Eq crumb, Walker c g, MonadCatch m) => crumb -> Rewrite c m g -> Rewrite c m g
childR n = focusR (childL n)
{-# INLINE childR #-}

-------------------------------------------------------------------------------

-- | Fold a tree in a top-down manner, using a single 'Translate' for each node.
foldtdT :: (Walker c g, MonadCatch m, Monoid b) => Translate c m g b -> Translate c m g b
foldtdT t = prefixFailMsg "foldtdT failed: " $
            let go = t `mappend` allT go
             in go
{-# INLINE foldtdT #-}

-- | Fold a tree in a bottom-up manner, using a single 'Translate' for each node.
foldbuT :: (Walker c g, MonadCatch m, Monoid b) => Translate c m g b -> Translate c m g b
foldbuT t = prefixFailMsg "foldbuT failed: " $
            let go = allT go `mappend` t
             in go
{-# INLINE foldbuT #-}

-- | Apply a 'Translate' to the first node for which it can succeed, in a top-down traversal.
onetdT :: (Walker c g, MonadCatch m) => Translate c m g b -> Translate c m g b
onetdT t = setFailMsg "onetdT failed" $
           let go = t <+ oneT go
            in go
{-# INLINE onetdT #-}

-- | Apply a 'Translate' to the first node for which it can succeed, in a bottom-up traversal.
onebuT :: (Walker c g, MonadCatch m) => Translate c m g b -> Translate c m g b
onebuT t = setFailMsg "onebuT failed" $
           let go = oneT go <+ t
            in go
{-# INLINE onebuT #-}

-- | Attempt to apply a 'Translate' in a top-down manner, pruning at successes.
prunetdT :: (Walker c g, MonadCatch m, Monoid b) => Translate c m g b -> Translate c m g b
prunetdT t = setFailMsg "prunetdT failed" $
             let go = t <+ allT go
              in go
{-# INLINE prunetdT #-}

-- | An always successful top-down fold, replacing failures with 'mempty'.
crushtdT :: (Walker c g, MonadCatch m, Monoid b) => Translate c m g b -> Translate c m g b
crushtdT t = foldtdT (mtryM t)
{-# INLINE crushtdT #-}

-- | An always successful bottom-up fold, replacing failures with 'mempty'.
crushbuT :: (Walker c g, MonadCatch m, Monoid b) => Translate c m g b -> Translate c m g b
crushbuT t = foldbuT (mtryM t)
{-# INLINE crushbuT #-}

-- | An always successful traversal that collects the results of all successful applications of a 'Translate' in a list.
collectT :: (Walker c g, MonadCatch m) => Translate c m g b -> Translate c m g [b]
collectT t = crushtdT (t >>^ singleton) >>^ toList
{-# INLINE collectT #-}

-- | Like 'collectT', but does not traverse below successes.
collectPruneT :: (Walker c g, MonadCatch m) => Translate c m g b -> Translate c m g [b]
collectPruneT t = prunetdT (t >>^ singleton) >>^ toList
{-# INLINE collectPruneT #-}

-------------------------------------------------------------------------------

-- | Apply a 'Rewrite' in a top-down manner, succeeding if they all succeed.
alltdR :: (Walker c g, MonadCatch m) => Rewrite c m g -> Rewrite c m g
alltdR r = prefixFailMsg "alltdR failed: " $
           let go = r >>> allR go
            in go
{-# INLINE alltdR #-}

-- | Apply a 'Rewrite' in a bottom-up manner, succeeding if they all succeed.
allbuR :: (Walker c g, MonadCatch m) => Rewrite c m g -> Rewrite c m g
allbuR r = prefixFailMsg "allbuR failed: " $
           let go = allR go >>> r
            in go
{-# INLINE allbuR #-}

-- | Apply a 'Rewrite' twice, in a top-down and bottom-up way, using one single tree traversal,
--   succeeding if they all succeed.
allduR :: (Walker c g, MonadCatch m) => Rewrite c m g -> Rewrite c m g
allduR r = prefixFailMsg "allduR failed: " $
           let go = r >>> allR go >>> r
            in go
{-# INLINE allduR #-}

-- | Apply a 'Rewrite' in a top-down manner, succeeding if any succeed.
anytdR :: (Walker c g, MonadCatch m) => Rewrite c m g -> Rewrite c m g
anytdR r = setFailMsg "anytdR failed" $
           let go = r >+> anyR go
            in go
{-# INLINE anytdR #-}

-- | Apply a 'Rewrite' in a bottom-up manner, succeeding if any succeed.
anybuR :: (Walker c g, MonadCatch m) => Rewrite c m g -> Rewrite c m g
anybuR r = setFailMsg "anybuR failed" $
           let go = anyR go >+> r
            in go
{-# INLINE anybuR #-}

-- | Apply a 'Rewrite' twice, in a top-down and bottom-up way, using one single tree traversal,
--   succeeding if any succeed.
anyduR :: (Walker c g, MonadCatch m) => Rewrite c m g -> Rewrite c m g
anyduR r = setFailMsg "anyduR failed" $
           let go = r >+> anyR go >+> r
            in go
{-# INLINE anyduR #-}

-- | Apply a 'Rewrite' to the first node for which it can succeed, in a top-down traversal.
onetdR :: (Walker c g, MonadCatch m) => Rewrite c m g -> Rewrite c m g
onetdR r = setFailMsg "onetdR failed" $
           let go = r <+ oneR go
            in go
{-# INLINE onetdR #-}

-- | Apply a 'Rewrite' to the first node for which it can succeed, in a bottom-up traversal.
onebuR :: (Walker c g, MonadCatch m) => Rewrite c m g -> Rewrite c m g
onebuR r = setFailMsg "onebuR failed" $
           let go = oneR go <+ r
            in go
{-# INLINE onebuR #-}

-- | Attempt to apply a 'Rewrite' in a top-down manner, pruning at successful rewrites.
prunetdR :: (Walker c g, MonadCatch m) => Rewrite c m g -> Rewrite c m g
prunetdR r = setFailMsg "prunetdR failed" $
             let go = r <+ anyR go
              in go
{-# INLINE prunetdR #-}

-- | A fixed-point traveral, starting with the innermost term.
innermostR :: (Walker c g, MonadCatch m) => Rewrite c m g -> Rewrite c m g
innermostR r = setFailMsg "innermostR failed" $
               let go = anybuR (r >>> tryR go)
                in go
{-# INLINE innermostR #-}

-------------------------------------------------------------------------------

tryL :: MonadCatch m => Lens c m g g -> Lens c m g g
tryL l = l `catchL` (\ _ -> id)
{-# INLINE tryL #-}

-- | Construct a 'Lens' by following a 'Path'.
pathL :: (ReadPath c crumb, Eq crumb, Walker c g, MonadCatch m) => Path crumb -> Lens c m g g
pathL = serialise . map childL
{-# INLINE pathL #-}

-- | Build a 'Lens' from the root to a point specified by a 'LocalPath'.
localPathL :: (ReadPath c crumb, Eq crumb, Walker c g, MonadCatch m) => LocalPath crumb -> Lens c m g g
localPathL = pathL . snocPathToPath
{-# INLINE localPathL #-}

-- | Construct a 'Lens' that points to the last node at which the 'Path' can be followed.
exhaustPathL :: (ReadPath c crumb, Eq crumb, Walker c g, MonadCatch m) => Path crumb -> Lens c m g g
exhaustPathL = foldr (\ n l -> tryL (childL n >>> l)) id
{-# INLINE exhaustPathL #-}

-- | Repeat as many iterations of the 'Path' as possible.
repeatPathL :: (ReadPath c crumb, Eq crumb, Walker c g, MonadCatch m) => Path crumb -> Lens c m g g
repeatPathL p = let go = tryL (pathL p >>> go)
                 in go
{-# INLINE repeatPathL #-}

-------------------------------------------------------------------------------

-- | Apply a 'Rewrite' at a point specified by a 'Path'.
pathR :: (ReadPath c crumb, Eq crumb, Walker c g, MonadCatch m) => Path crumb -> Rewrite c m g -> Rewrite c m g
pathR = focusR . pathL
{-# INLINE pathR #-}

-- | Apply a 'Translate' at a point specified by a 'Path'.
pathT :: (ReadPath c crumb, Eq crumb, Walker c g, MonadCatch m) => Path crumb -> Translate c m g b -> Translate c m g b
pathT = focusT . pathL
{-# INLINE pathT #-}

-- | Apply a 'Rewrite' at a point specified by a 'LocalPath'.
localPathR :: (ReadPath c crumb, Eq crumb, Walker c g, MonadCatch m) => LocalPath crumb -> Rewrite c m g -> Rewrite c m g
localPathR = focusR . localPathL
{-# INLINE localPathR #-}

-- | Apply a 'Translate' at a point specified by a 'LocalPath'.
localPathT :: (ReadPath c crumb, Eq crumb, Walker c g, MonadCatch m) => LocalPath crumb -> Translate c m g b -> Translate c m g b
localPathT = focusT . localPathL
{-# INLINE localPathT #-}

-------------------------------------------------------------------------------

-- | Check if it is possible to construct a 'Lens' along this path from the current node.
testPathT :: (ReadPath c crumb, Eq crumb, Walker c g, MonadCatch m) => Path crumb -> Translate c m g Bool
testPathT = testLensT . pathL
{-# INLINE testPathT #-}

-------------------------------------------------------------------------------

-- | Apply a 'Rewrite' to the largest node(s) that satisfy the predicate, requiring all to succeed.
allLargestR :: (Walker c g, MonadCatch m) => Translate c m g Bool -> Rewrite c m g -> Rewrite c m g
allLargestR p r = prefixFailMsg "allLargestR failed: " $
                  let go = ifM p r (allR go)
                   in go
{-# INLINE allLargestR #-}

-- | Apply a 'Rewrite' to the largest node(s) that satisfy the predicate, succeeding if any succeed.
anyLargestR :: (Walker c g, MonadCatch m) => Translate c m g Bool -> Rewrite c m g -> Rewrite c m g
anyLargestR p r = setFailMsg "anyLargestR failed" $
                  let go = ifM p r (anyR go)
                   in go
{-# INLINE anyLargestR #-}

-- | Apply a 'Rewrite' to the first node for which it can succeed among the largest node(s) that satisfy the predicate.
oneLargestR :: (Walker c g, MonadCatch m) => Translate c m g Bool -> Rewrite c m g -> Rewrite c m g
oneLargestR p r = setFailMsg "oneLargestR failed" $
                  let go = ifM p r (oneR go)
                   in go
{-# INLINE oneLargestR #-}

-- | Apply a 'Translate' to the largest node(s) that satisfy the predicate, combining the results in a monoid.
allLargestT :: (Walker c g, MonadCatch m, Monoid b) => Translate c m g Bool -> Translate c m g b -> Translate c m g b
allLargestT p t = prefixFailMsg "allLargestT failed: " $
                  let go = ifM p t (allT go)
                   in go
{-# INLINE allLargestT #-}

-- | Apply a 'Translate' to the first node for which it can succeed among the largest node(s) that satisfy the predicate.
oneLargestT :: (Walker c g, MonadCatch m) => Translate c m g Bool -> Translate c m g b -> Translate c m g b
oneLargestT p t = setFailMsg "oneLargestT failed" $
                  let go = ifM p t (oneT go)
                   in go
{-# INLINE oneLargestT #-}

-- | Test if the type of the current node summand matches the type of the argument.
--   Note that the argument /value/ is never inspected, it is merely a proxy for a type argument.
summandIsTypeT :: forall c m a g. (MonadCatch m, Injection a g) => a -> Translate c m g Bool
summandIsTypeT _ = arr (isJust . (project :: (g -> Maybe a)))
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

instance (Monoid w, Monad m) => Monad (AllT w m) where
-- return :: a -> AllT w m a
   return a = AllT $ return (P a mempty)
   {-# INLINE return #-}

-- fail :: String -> AllT w m a
   fail = AllT . fail
   {-# INLINE fail #-}

-- (>>=) :: AllT w m a -> (a -> AllT w m d) -> AllT w m d
   ma >>= f = AllT $ do P a w1 <- unAllT ma
                        P d w2 <- unAllT (f a)
                        return (P d (w1 <> w2))
   {-# INLINE (>>=) #-}

instance (Monoid w, MonadCatch m) => MonadCatch (AllT w m) where
-- catchM :: AllT w m a -> (String -> AllT w m a) -> AllT w m a
   catchM (AllT ma) f = AllT $ ma `catchM` (unAllT . f)
   {-# INLINE catchM #-}


-- | Wrap a 'Translate' using the 'AllT' monad transformer.
wrapAllT :: Monad m => Translate c m g b -> Rewrite c (AllT b m) g
wrapAllT t = readerT $ \ a -> resultT (AllT . liftM (P a)) t
{-# INLINE wrapAllT #-}

-- | Unwrap a 'Translate' from the 'AllT' monad transformer.
unwrapAllT :: MonadCatch m => Rewrite c (AllT b m) g -> Translate c m g b
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

instance Monad m => Monad (OneT w m) where
-- return :: a -> OneT w m a
   return a = OneT $ \ mw -> return (P a mw)
   {-# INLINE return #-}

-- fail :: String -> OneT w m a
   fail msg = OneT (\ _ -> fail msg)
   {-# INLINE fail #-}

-- (>>=) :: OneT w m a -> (a -> OneT w m d) -> OneT w m d
   ma >>= f = OneT $ do \ mw1 -> do P a mw2 <- unOneT ma mw1
                                    unOneT (f a) mw2
   {-# INLINE (>>=) #-}

instance MonadCatch m => MonadCatch (OneT w m) where
-- catchM :: OneT w m a -> (String -> OneT w m a) -> OneT w m a
   catchM (OneT g) f = OneT $ \ mw -> g mw `catchM` (($ mw) . unOneT . f)
   {-# INLINE catchM #-}


-- | Wrap a 'Translate' using the 'OneT' monad transformer.
wrapOneT :: MonadCatch m => Translate c m g b -> Rewrite c (OneT b m) g
wrapOneT t = rewrite $ \ c a -> OneT $ \ mw -> case mw of
                                                 Just w  -> return (P a (Just w))
                                                 Nothing -> ((P a . Just) `liftM` apply t c a) <+ return (P a mw)
{-# INLINE wrapOneT #-}

-- | Unwrap a 'Translate' from the 'OneT' monad transformer.
unwrapOneT :: Monad m => Rewrite c (OneT b m) g -> Translate c m g b
unwrapOneT = resultT (checkSuccessPMaybe "oneT failed" . liftM pSnd . ($ Nothing) . unOneT)
{-# INLINE unwrapOneT #-}

-------------------------------------------------------------------------------

-- If allR just used Monad (rather than MonadCatch), this (and other things) would be simpler.
-- And currently, the only use of MonadCatch is that it allows the error message to be modified.

-- Failure should not occur, so it doesn't really matter where the KureM monad sits in the GetChild stack.
-- I've arbitrarily made it a local failure.

data GetChild c g a = GetChild (KureM a) (Maybe (c,g))

getChildSecond :: (Maybe (c,g) -> Maybe (c,g)) -> GetChild c g a -> GetChild c g a
getChildSecond f (GetChild ka mcg) = GetChild ka (f mcg)
{-# INLINE getChildSecond #-}

instance Monad (GetChild c g) where
-- return :: a -> GetChild c g a
   return a = GetChild (return a) Nothing
   {-# INLINE return #-}

-- fail :: String -> GetChild c g a
   fail msg = GetChild (fail msg) Nothing
   {-# INLINE fail #-}

-- (>>=) :: GetChild c g a -> (a -> GetChild c g b) -> GetChild c g b
   (GetChild kma mcg) >>= k = runKureM (\ a   -> getChildSecond (mplus mcg) (k a))
                                       (\ msg -> GetChild (fail msg) mcg)
                                       kma
   {-# INLINE (>>=) #-}

instance MonadCatch (GetChild c g) where
-- catchM :: GetChild c g a -> (String -> GetChild c g a) -> GetChild c g a
   gc@(GetChild kma mcg) `catchM` k = runKureM (\ _   -> gc)
                                               (\ msg -> getChildSecond (mplus mcg) (k msg))
                                               kma
   {-# INLINE catchM #-}


wrapGetChild :: (ReadPath c crumb, Eq crumb) => crumb -> Rewrite c (GetChild c g) g
wrapGetChild cr = do cr' <- lastCrumbT
                     rewrite $ \ c a -> GetChild (return a) (if cr == cr' then Just (c, a) else Nothing)
{-# INLINE wrapGetChild #-}

unwrapGetChild :: Rewrite c (GetChild c g) g -> Translate c Maybe g (c,g)
unwrapGetChild = resultT (\ (GetChild _ mcg) -> mcg)
{-# INLINE unwrapGetChild #-}

getChild :: (ReadPath c crumb, Eq crumb, Walker c g) => crumb -> Translate c Maybe g (c, g)
getChild = unwrapGetChild . allR . wrapGetChild
{-# INLINE getChild #-}

-------------------------------------------------------------------------------

type SetChild = KureM

wrapSetChild :: (ReadPath c crumb, Eq crumb) => crumb -> g -> Rewrite c SetChild g
wrapSetChild cr g = do cr' <- lastCrumbT
                       if cr == cr' then return g else idR
{-# INLINE wrapSetChild #-}

unwrapSetChild :: Monad m => Rewrite c SetChild g -> Rewrite c m g
unwrapSetChild = resultT (runKureM return fail)
{-# INLINE unwrapSetChild #-}

setChild :: (ReadPath c crumb, Eq crumb, Walker c g, Monad m) => crumb -> g -> Rewrite c m g
setChild cr = unwrapSetChild . allR . wrapSetChild cr
{-# INLINE setChild #-}

-------------------------------------------------------------------------------

childL_default :: forall c crumb m g. (ReadPath c crumb, Eq crumb) => (Walker c g, MonadCatch m) => crumb -> Lens c m g g
childL_default cr = lens $ do cg <- getter
                              k  <- setter
                              return (cg, k)
  where
    getter :: Translate c m g (c,g)
    getter = resultT (projectWithFailMsgM "there is no child matching the crumb.") (getChild cr)
    {-# INLINE getter #-}

    setter :: Translate c m g (g -> m g)
    setter = translate $ \ c a -> return (\ b -> apply (setChild cr b) c a)
    {-# INLINE setter #-}
{-# INLINE childL_default #-}

-------------------------------------------------------------------------------
