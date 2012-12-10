{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}

-- |
-- Module: Language.KURE.Walker
-- Copyright: (c) 2012 The University of Kansas
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
        , numChildrenT
        , hasChildT
        , summandIsTypeT

        -- * Paths
        -- ** Absolute Paths
        , AbsolutePath
        , rootAbsPath
        , extendAbsPath
        , PathContext(..)
        , absPathT
        -- ** Relative Paths
        , Path
        , rootPath
        , rootPathT
        , pathsToT
        , onePathToT
        , oneNonEmptyPathToT
        , prunePathsToT
        , uniquePathToT
        , uniquePrunePathToT

        -- ** Building Lenses from Paths
        , pathL
        , exhaustPathL
        , repeatPathL
        , rootL

        -- ** Applying transformations at the end of Paths
        , pathR
        , pathT

        -- ** Testing Paths
        , testPathT
) where

import Prelude hiding (id)

import Data.Maybe (isJust)
import Data.Monoid
import Data.List

import Control.Monad
import Control.Arrow
import Control.Category hiding ((.))

import Language.KURE.Catch
import Language.KURE.Translate
import Language.KURE.Lens
import Language.KURE.Injection
import Language.KURE.Combinators

-------------------------------------------------------------------------------

-- | 'Walker' captures the ability to walk over a tree containing nodes of type @g@,
--   using a specific context @c@.
--
--   Minimal complete definition: 'allR'.
--
--   Default definitions are provided for 'anyR', 'oneR', 'allT', 'oneT', and 'childL',
--   but they may be overridden for efficiency.

class Walker c g where

  -- | Apply a 'Generic' 'Rewrite' to all immediate children, succeeding if they all succeed.
  allR :: MonadCatch m => Rewrite c m g -> Rewrite c m g

  -- | Apply a 'Generic' 'Translate' to all immediate children, succeeding if they all succeed.
  --   The results are combined in a 'Monoid'.
  allT :: (MonadCatch m, Monoid b) => Translate c m g b -> Translate c m g b
  allT = unwrapAllT . allR . wrapAllT
  {-# INLINE allT #-}

  -- | Apply a 'Generic' 'Translate' to the first immediate child for which it can succeed.
  oneT :: MonadCatch m => Translate c m g b -> Translate c m g b
  oneT = unwrapOneT . allR . wrapOneT
  {-# INLINE oneT #-}

  -- | Apply a 'Generic' 'Rewrite' to all immediate children, suceeding if any succeed.
  anyR :: MonadCatch m => Rewrite c m g -> Rewrite c m g
  anyR = unwrapAnyR . allR . wrapAnyR
  {-# INLINE anyR #-}

  -- | Apply a 'Generic' 'Rewrite' to the first immediate child for which it can succeed.
  oneR :: MonadCatch m => Rewrite c m g -> Rewrite c m g
  oneR = unwrapOneR . allR . wrapOneR
  {-# INLINE oneR #-}

  -- | Construct a 'Lens' to the n-th child 'Node'.
  childL :: MonadCatch m => Int -> Lens c m g g
  childL = childL_default
  {-# INLINE childL #-}

------------------------------------------------------------------------------------------

-- | Lifted version of 'numChildren'.
numChildrenT :: (Walker c g, MonadCatch m) => Translate c m g Int
numChildrenT = getSum `liftM` allT (return $ Sum 1)
{-# INLINE numChildrenT #-}

-- | Lifted version of 'hasChild'.
hasChildT :: (Walker c g, MonadCatch m) => Int -> Translate c m g Bool
hasChildT n = do c <- numChildrenT
                 return (n >= 0 && n < c)
{-# INLINE hasChildT #-}

-------------------------------------------------------------------------------

-- | Apply a 'Translate' to a specified child.
childT :: (Walker c g, MonadCatch m) => Int -> Translate c m g b -> Translate c m g b
childT n = focusT (childL n)
{-# INLINE childT #-}

-- | Apply a 'Rewrite' to a specified child.
childR :: (Walker c g, MonadCatch m) => Int -> Rewrite c m g -> Rewrite c m g
childR n = focusR (childL n)
{-# INLINE childR #-}

-------------------------------------------------------------------------------

-- | Fold a tree in a top-down manner, using a single 'Translate' for each 'Node'.
foldtdT :: (Walker c g, MonadCatch m, Monoid b) => Translate c m g b -> Translate c m g b
foldtdT t = prefixFailMsg "foldtdT failed: " $
            let go = t `mappend` allT go
             in go
{-# INLINE foldtdT #-}

-- | Fold a tree in a bottom-up manner, using a single 'Translate' for each 'Node'.
foldbuT :: (Walker c g, MonadCatch m, Monoid b) => Translate c m g b -> Translate c m g b
foldbuT t = prefixFailMsg "foldbuT failed: " $
            let go = allT go `mappend` t
             in go
{-# INLINE foldbuT #-}

-- | Apply a 'Translate' to the first 'Node' for which it can succeed, in a top-down traversal.
onetdT :: (Walker c g, MonadCatch m) => Translate c m g b -> Translate c m g b
onetdT t = setFailMsg "onetdT failed" $
           let go = t <+ oneT go
            in go
{-# INLINE onetdT #-}

-- | Apply a 'Translate' to the first 'Node' for which it can succeed, in a bottom-up traversal.
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
collectT t = crushtdT (t >>^ return)
{-# INLINE collectT #-}

-- | Like 'collectT', but does not traverse below successes.
collectPruneT :: (Walker c g, MonadCatch m) => Translate c m g b -> Translate c m g [b]
collectPruneT t = prunetdT (t >>^ return)
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

-- | Apply a 'Rewrite' to the first 'Node' for which it can succeed, in a top-down traversal.
onetdR :: (Walker c g, MonadCatch m) => Rewrite c m g -> Rewrite c m g
onetdR r = setFailMsg "onetdR failed" $
           let go = r <+ oneR go
            in go
{-# INLINE onetdR #-}

-- | Apply a 'Rewrite' to the first 'Node' for which it can succeed, in a bottom-up traversal.
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

-- | A path from the root.
newtype AbsolutePath = AbsolutePath [Int] deriving Eq

instance Show AbsolutePath where
  show (AbsolutePath p) = show (reverse p)
  {-# INLINE show #-}

-- | The (empty) 'AbsolutePath' to the root.
rootAbsPath :: AbsolutePath
rootAbsPath = AbsolutePath []
{-# INLINE rootAbsPath #-}

-- | Extend an 'AbsolutePath' by one descent.
extendAbsPath :: Int -> AbsolutePath -> AbsolutePath
extendAbsPath n (AbsolutePath ns) = AbsolutePath (n:ns)
{-# INLINE extendAbsPath #-}

-- | Contexts that are instances of 'PathContext' contain the current 'AbsolutePath'.
--   Any user-defined combinators (typically 'allR' and congruence combinators) should update the 'AbsolutePath' using '@@'.
class PathContext c where
  -- | Retrieve the current absolute path.
  absPath :: c -> AbsolutePath

  -- | Extend the current absolute path.
  (@@) :: c -> Int -> c

-- | The simplest instance of 'PathContext' is 'AbsolutePath' itself.
instance PathContext AbsolutePath where
-- absPath :: AbsolutePath -> AbsolutePath
   absPath p = p
   {-# INLINE absPath #-}

-- (@@) :: AbsolutePath -> Int -> AbsolutePath
   (@@) = flip extendAbsPath
   {-# INLINE (@@) #-}

-- | Lifted version of 'absPath'.
absPathT :: (PathContext c, Monad m) => Translate c m a AbsolutePath
absPathT = absPath `liftM` contextT
{-# INLINE absPathT #-}

-------------------------------------------------------------------------------

-- | A path is a route to descend the tree from an arbitrary 'Node'.
type Path = [Int]

-- | Retrieve the 'Path' from the root to the current node.
rootPath :: PathContext c => c -> Path
rootPath c = let AbsolutePath p = absPath c
              in reverse p
{-# INLINE rootPath #-}

-- | Lifted version of 'rootPath'.
rootPathT :: (PathContext c, Monad m) => Translate c m a Path
rootPathT = rootPath `liftM` contextT
{-# INLINE rootPathT #-}

--  Provided the first 'AbsolutePath' is a prefix of the second 'AbsolutePath',
--  computes the 'Path' from the end of the first to the end of the second.
rmPathPrefix :: AbsolutePath -> AbsolutePath -> Maybe Path
rmPathPrefix (AbsolutePath p1) (AbsolutePath p2) = do guard (p1 `isSuffixOf` p2)
                                                      return $ drop (length p1) (reverse p2)
{-# INLINE rmPathPrefix #-}

--  Construct a 'Path' from the current 'Node' to the end of the given 'AbsolutePath', provided that 'AbsolutePath' passes through the current 'Node'.
abs2pathT :: (PathContext c, Monad m) => AbsolutePath -> Translate c m a Path
abs2pathT there = do here <- absPathT
                     maybe (fail "Absolute path does not pass through current node.") return (rmPathPrefix here there)
{-# INLINE abs2pathT #-}

-- | Find the 'Path's to every 'Node' that satisfies the predicate.
pathsToT :: (PathContext c, Walker c g, MonadCatch m) => (g -> Bool) -> Translate c m g [Path]
pathsToT q = collectT (acceptR q >>> absPathT) >>= mapM abs2pathT
{-# INLINE pathsToT #-}

-- | Find the 'Path' to the first 'Node' that satisfies the predicate (in a pre-order traversal).
onePathToT :: (PathContext c, Walker c g, MonadCatch m) => (g -> Bool) -> Translate c m g Path
onePathToT q = setFailMsg "No matching nodes found." $
               onetdT (acceptR q >>> absPathT) >>= abs2pathT
{-# INLINE onePathToT #-}

-- | Find the 'Path' to the first descendent 'Node' that satisfies the predicate (in a pre-order traversal).
oneNonEmptyPathToT :: (PathContext c, Walker c g, MonadCatch m) => (g -> Bool) -> Translate c m g Path
oneNonEmptyPathToT q = setFailMsg "No matching nodes found." $
                       do start <- absPathT
                          onetdT (acceptR q >>> absPathT >>> acceptR (/= start)) >>= abs2pathT
{-# INLINE oneNonEmptyPathToT #-}

-- | Find the 'Path's to every 'Node' that satisfies the predicate, ignoring 'Node's below successes.
prunePathsToT :: (PathContext c, Walker c g, MonadCatch m) => (g -> Bool) -> Translate c m g [Path]
prunePathsToT q = collectPruneT (acceptR q >>> absPathT) >>= mapM abs2pathT
{-# INLINE prunePathsToT #-}

-- local function used by uniquePathToT and uniquePrunePathToT
requireUniquePath :: Monad m => Translate c m [Path] Path
requireUniquePath = contextfreeT $ \ ps -> case ps of
                                             []  -> fail "No matching nodes found."
                                             [p] -> return p
                                             _   -> fail $ "Ambiguous: " ++ show (length ps) ++ " matching nodes found."
{-# INLINE requireUniquePath #-}

-- | Find the 'Path' to the 'Node' that satisfies the predicate, failing if that does not uniquely identify a 'Node'.
uniquePathToT :: (PathContext c, Walker c g, MonadCatch m) => (g -> Bool) -> Translate c m g Path
uniquePathToT q = pathsToT q >>> requireUniquePath
{-# INLINE uniquePathToT #-}

-- | Build a 'Path' to the 'Node' that satisfies the predicate, failing if that does not uniquely identify a 'Node' (ignoring 'Node's below successes).
uniquePrunePathToT :: (PathContext c, Walker c g, MonadCatch m) => (g -> Bool) -> Translate c m g Path
uniquePrunePathToT q = prunePathsToT q >>> requireUniquePath
{-# INLINE uniquePrunePathToT #-}

-------------------------------------------------------------------------------

-- | Construct a 'Lens' by following a 'Path'.
pathL :: (Walker c g, MonadCatch m) => Path -> Lens c m g g
pathL = foldr (>>>) id . map childL
{-# INLINE pathL #-}

-- | Construct a 'Lens' that points to the last 'Node' at which the 'Path' can be followed.
exhaustPathL :: (Walker c g, MonadCatch m) => Path -> Lens c m g g
exhaustPathL = foldr (\ n l -> tryR (childL n >>> l)) id
{-# INLINE exhaustPathL #-}

-- | Repeat as many iterations of the 'Path' as possible.
repeatPathL :: (Walker c g, MonadCatch m) => Path -> Lens c m g g
repeatPathL p = let go = tryR (pathL p >>> go)
                 in go
{-# INLINE repeatPathL #-}

-- | Build a 'Lens' from the root to a point specified by an 'AbsolutePath'.
rootL :: (Walker c g, MonadCatch m) => AbsolutePath -> Lens c m g g
rootL = pathL . rootPath
{-# INLINE rootL #-}

-------------------------------------------------------------------------------

-- | Apply a 'Rewrite' at a point specified by a 'Path'.
pathR :: (Walker c g, MonadCatch m) => Path -> Rewrite c m g -> Rewrite c m g
pathR = focusR . pathL
{-# INLINE pathR #-}

-- | Apply a 'Translate' at a point specified by a 'Path'.
pathT :: (Walker c g, MonadCatch m) => Path -> Translate c m g b -> Translate c m g b
pathT = focusT . pathL
{-# INLINE pathT #-}

-------------------------------------------------------------------------------

-- | Check if it is possible to construct a 'Lens' along this path from the current 'Node'.
testPathT :: (Walker c g, MonadCatch m) => Path -> Translate c m g Bool
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

-- | Test if the type of the current ('Generic') 'Node' matches the type of the argument.
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

-- $AllT_doc
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

-- $OneT_doc
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
                                                 Nothing -> ((P a . Just) `liftM` apply t c a) <<+ return (P a mw)
{-# INLINE wrapOneT #-}

-- | Unwrap a 'Translate' from the 'OneT' monad transformer.
unwrapOneT :: Monad m => Rewrite c (OneT b m) g -> Translate c m g b
unwrapOneT = resultT (checkSuccessPMaybe "oneT failed" . liftM pSnd . ($ Nothing) . unOneT)
{-# INLINE unwrapOneT #-}

-------------------------------------------------------------------------------

data PInt a = PInt {-# UNPACK #-} !Int a

secondPInt :: (a -> b) -> PInt a -> PInt b
secondPInt f = \ (PInt i a) -> PInt i (f a)
{-# INLINE secondPInt #-}

-------------------------------------------------------------------------------

-- This is hideous.
-- Admittedly, part of the problem is using MonadCatch.  If allR just used Monad, this (and other things) would be much simpler.
-- And currently, the only use of MonadCatch is that it allows the error message to be modified.

-- Failure should not occur, so it doesn't really matter where the KureM monad sits in the GetChild stack.
-- I've arbitrarily made it a local failure.

newtype GetChild c g a = GetChild (Int -> PInt (KureM a, Maybe (c,g)))

unGetChild :: GetChild c g a -> Int -> PInt (KureM a, Maybe (c,g))
unGetChild (GetChild f) = f
{-# INLINE unGetChild #-}

instance Monad (GetChild c g) where
-- return :: a -> GetChild c g a
   return a = GetChild $ \ i -> PInt i (return a, Nothing)
   {-# INLINE return #-}

-- fail :: String -> GetChild c g a
   fail msg = GetChild $ \ i -> PInt i (fail msg, Nothing)
   {-# INLINE fail #-}

-- (>>=) :: GetChild c g a -> (a -> GetChild c g b) -> GetChild c g b
   ma >>= f = GetChild $ \ i0 -> let PInt i1 (kma, mcg) = unGetChild ma i0
                                  in runKureM (\ a   -> (secondPInt.second) (mplus mcg) $ unGetChild (f a) i1)
                                              (\ msg -> PInt i1 (fail msg, mcg))
                                              kma
   {-# INLINE (>>=) #-}

instance MonadCatch (GetChild c g) where
-- catchM :: GetChild c g a -> (String -> GetChild c g a) -> GetChild c g a
   ma `catchM` f = GetChild $ \ i0 -> let p@(PInt i1 (kma, mcg)) = unGetChild ma i0
                                       in runKureM (\ _   -> p)
                                                   (\ msg -> (secondPInt.second) (mplus mcg) $ unGetChild (f msg) i1)
                                                   kma
   {-# INLINE catchM #-}


wrapGetChild :: Int -> Rewrite c (GetChild c g) g
wrapGetChild n = rewrite $ \ c a -> GetChild $ \ m -> PInt (m + 1)
                                                           (return a, if n == m then Just (c, a) else Nothing)
{-# INLINE wrapGetChild #-}

unwrapGetChild :: Rewrite c (GetChild c g) g -> Translate c Maybe g (c,g)
unwrapGetChild r = translate $ \ c a -> let PInt _ (_,mcg) = unGetChild (apply r c a) 0
                                         in mcg
{-# INLINE unwrapGetChild #-}

getChild :: Walker c g => Int -> Translate c Maybe g (c, g)
getChild = unwrapGetChild . allR . wrapGetChild
{-# INLINE getChild #-}

-------------------------------------------------------------------------------

newtype SetChild a = SetChild (Int -> PInt (KureM a))

unSetChild :: SetChild a -> Int -> PInt (KureM a)
unSetChild (SetChild f) = f
{-# INLINE unSetChild #-}

instance Monad SetChild where
-- return :: a -> SetChild c g a
   return a = SetChild $ \ i -> PInt i (return a)
   {-# INLINE return #-}

-- fail :: String -> SetChild c g a
   fail msg = SetChild $ \ i -> PInt i (fail msg)
   {-# INLINE fail #-}

-- (>>=) :: SetChild c g a -> (a -> SetChild c g b) -> SetChild c g b
   ma >>= f = SetChild $ \ i0 -> let PInt i1 ka = unSetChild ma i0
                                  in runKureM (\ a   -> unSetChild (f a) i1)
                                              (\ msg -> PInt i1 (fail msg))
                                              ka
   {-# INLINE (>>=) #-}

instance MonadCatch SetChild where
-- catchM :: SetChild c g a -> (String -> SetChild c g a) -> SetChild c g a
   ma `catchM` f = SetChild $ \ i0 -> let PInt i1 ka = unSetChild ma i0
                                       in runKureM (\ _   -> PInt i1 ka)
                                                   (\ msg -> unSetChild (f msg) i1)
                                                   ka
   {-# INLINE catchM #-}


wrapSetChild :: Int -> g -> Rewrite c SetChild g
wrapSetChild n g = contextfreeT $ \ a -> SetChild $ \ m -> PInt (m + 1)
                                                                (return $ if n == m then g else a)
{-# INLINE wrapSetChild #-}

unwrapSetChild :: Monad m => Rewrite c SetChild g -> Rewrite c m g
unwrapSetChild r = rewrite $ \ c a -> let PInt _ ka = unSetChild (apply r c a) 0
                                       in runKureM return fail ka
{-# INLINE unwrapSetChild #-}

setChild :: (Walker c g, Monad m) => Int -> g -> Rewrite c m g
setChild n = unwrapSetChild . allR . wrapSetChild n
{-# INLINE setChild #-}

-------------------------------------------------------------------------------

childL_default :: forall c m g. (Walker c g, MonadCatch m) => Int -> Lens c m g g
childL_default n = lens $ do cg <- getter
                             k  <- setter
                             return (cg, k)
  where
    getter :: Translate c m g (c,g)
    getter = translate $ \ c a -> maybe (fail $ "there is no child number " ++ show n) return (apply (getChild n) c a)
    {-# INLINE getter #-}

    setter :: Translate c m g (g -> m g)
    setter = translate $ \ c a -> return (\ b -> apply (setChild n b) c a)
    {-# INLINE setter #-}

{-# INLINE childL_default #-}

-------------------------------------------------------------------------------
