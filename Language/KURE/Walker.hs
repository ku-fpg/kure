{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, ScopedTypeVariables, ConstraintKinds #-}

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
        ( -- * Nodes
          Node(..)
        , numChildrenT
        , hasChildT

        -- * Tree Walkers
        , Walker

        -- * Rewrite Traversals
        , childR
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

        -- * Translate Traversals
        , childT
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
        , pathsToT
        , onePathToT
        , oneNonEmptyPathToT
        , prunePathsToT
        , uniquePathToT
        , uniquePrunePathToT

        -- * Using Paths
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

import Language.KURE.Combinators
import Language.KURE.Translate
import Language.KURE.Injection

------------------------------------------------------------------------------------------

-- | Lifted version of 'numChildren'.
numChildrenT :: Walker c m g => Translate c m g Int
numChildrenT = getSum `liftM` allT (return $ Sum 1)
{-# INLINE numChildrenT #-}

-- | Lifted version of 'hasChild'.
hasChildT :: Walker c m g => Int -> Translate c m g Bool
hasChildT n = do c <- numChildrenT
                 return (n >= 0 && n < c)
{-# INLINE hasChildT #-}

-------------------------------------------------------------------------------

-- | 'Walker' captures the ability to walk over a tree of 'Node's,
--   using a specific context @c@ and a 'MonadCatch' @m@.
--
--   Minimal complete definition: 'childL'.
--
--   Default definitions are provided for 'allT', 'oneT', 'allR', 'anyR' and 'oneR', but they may be overridden for efficiency.
--   The typical situation for which overwriting is useful is when visiting each child in one traversal of the node is more efficient than using a separate traversal to visit each child (which is the default behaviour).
--   For example, if the node stores a list of children.

-- | A 'Node' is any node in the tree that you wish to be able to traverse.

class Node c g where

  -- | Construct a 'MultiLens' to the children of the current 'Node'.
  childrenL :: MonadCatch m => MultiLens c m g g

  -- | Apply a 'Generic' 'Translate' to all immediate children, succeeding if they all succeed.
  --   The results are combined in a 'Monoid'.
  allT :: (MonadCatch m, Monoid b) => Translate c m g b -> Translate c m g b
  allT t = prefixFailMsg "allT failed: " $
           allFocusT childrenL t
  {-# INLINE allT #-}

  -- | Apply a 'Generic' 'Translate' to the first immediate child for which it can succeed.
  oneT :: MonadCatch m => Translate c m g b -> Translate c m g b
  oneT t = setFailMsg "oneT failed" $
           oneFocusT childrenL t
  {-# INLINE oneT #-}

  -- | Apply a 'Generic' 'Rewrite' to all immediate children, succeeding if they all succeed.
  allR :: MonadCatch m => Rewrite c m g -> Rewrite c m g
  allR r = prefixFailMsg "allR failed: " $
           allFocusR childrenL r
  {-# INLINE allR #-}

  -- | Apply a 'Generic' 'Rewrite' to all immediate children, suceeding if any succeed.
  anyR :: MonadCatch m => Rewrite c m g -> Rewrite c m g
  anyR r = setFailMsg "anyR failed" $
           anyFocusR childrenL r
  {-# INLINE anyR #-}

  -- | Apply a 'Generic' 'Rewrite' to the first immediate child for which it can succeed.
  oneR :: MonadCatch m => Rewrite c m g -> Rewrite c m g
  oneR r = setFailMsg "oneR failed" $
           oneFocusR childrenL r
  {-# INLINE oneR #-}

type Walker c m g = (Node c g, MonadCatch m)

-------------------------------------------------------------------------------

-- | Construct a 'Lens' to the n-th child 'Node'.
childL :: Walker c m g => Int -> Lens c m g g
childL n = indexL n childrenL
{-# INLINE childL #-}

-- | Apply a 'Translate' to a specified child.
childT :: Walker c m g => Int -> Translate c m g b -> Translate c m g b
childT n = focusT (childL n)
{-# INLINE childT #-}

-- | Apply a 'Rewrite' to a specified child.
childR :: Walker c m g => Int -> Rewrite c m g -> Rewrite c m g
childR n = focusR (childL n)
{-# INLINE childR #-}

-------------------------------------------------------------------------------

-- | Fold a tree in a top-down manner, using a single 'Translate' for each 'Node'.
foldtdT :: (Walker c m g, Monoid b) => Translate c m g b -> Translate c m g b
foldtdT t = prefixFailMsg "foldtdT failed: " $
            let go = t `mappend` allT go
             in go
{-# INLINE foldtdT #-}

-- | Fold a tree in a bottom-up manner, using a single 'Translate' for each 'Node'.
foldbuT :: (Walker c m g, Monoid b) => Translate c m g b -> Translate c m g b
foldbuT t = prefixFailMsg "foldbuT failed: " $
            let go = allT go `mappend` t
             in go
{-# INLINE foldbuT #-}

-- | Apply a 'Translate' to the first 'Node' for which it can succeed, in a top-down traversal.
onetdT :: (Walker c m g) => Translate c m g b -> Translate c m g b
onetdT t = setFailMsg "onetdT failed" $
           let go = t <+ oneT go
            in go
{-# INLINE onetdT #-}

-- | Apply a 'Translate' to the first 'Node' for which it can succeed, in a bottom-up traversal.
onebuT :: (Walker c m g) => Translate c m g b -> Translate c m g b
onebuT t = setFailMsg "onetdT failed" $
           let go = oneT go <+ t
            in go
{-# INLINE onebuT #-}

-- | Attempt to apply a 'Translate' in a top-down manner, pruning at successes.
prunetdT :: (Walker c m g, Monoid b) => Translate c m g b -> Translate c m g b
prunetdT t = setFailMsg "prunetdT failed" $
             let go = t <+ allT go
              in go
{-# INLINE prunetdT #-}

-- | An always successful top-down fold, replacing failures with 'mempty'.
crushtdT :: (Walker c m g, Monoid b) => Translate c m g b -> Translate c m g b
crushtdT t = foldtdT (mtryM t)
{-# INLINE crushtdT #-}

-- | An always successful bottom-up fold, replacing failures with 'mempty'.
crushbuT :: (Walker c m g, Monoid b) => Translate c m g b -> Translate c m g b
crushbuT t = foldbuT (mtryM t)
{-# INLINE crushbuT #-}

-- | An always successful traversal that collects the results of all successful applications of a 'Translate' in a list.
collectT :: (Walker c m g) => Translate c m g b -> Translate c m g [b]
collectT t = crushtdT (t >>^ return)
{-# INLINE collectT #-}

-- | Like 'collectT', but does not traverse below successes.
collectPruneT :: (Walker c m g) => Translate c m g b -> Translate c m g [b]
collectPruneT t = prunetdT (t >>^ return)
{-# INLINE collectPruneT #-}

-------------------------------------------------------------------------------

-- | Apply a 'Rewrite' in a top-down manner, succeeding if they all succeed.
alltdR :: (Walker c m g) => Rewrite c m g -> Rewrite c m g
alltdR r = prefixFailMsg "alltdR failed: " $
           let go = r >>> allR go
            in go
{-# INLINE alltdR #-}

-- | Apply a 'Rewrite' in a bottom-up manner, succeeding if they all succeed.
allbuR :: (Walker c m g) => Rewrite c m g -> Rewrite c m g
allbuR r = prefixFailMsg "allbuR failed: " $
           let go = allR go >>> r
            in go
{-# INLINE allbuR #-}

-- | Apply a 'Rewrite' twice, in a top-down and bottom-up way, using one single tree traversal,
--   succeeding if they all succeed.
allduR :: (Walker c m g) => Rewrite c m g -> Rewrite c m g
allduR r = prefixFailMsg "allduR failed: " $
           let go = r >>> allR go >>> r
            in go
{-# INLINE allduR #-}

-- | Apply a 'Rewrite' in a top-down manner, succeeding if any succeed.
anytdR :: (Walker c m g) => Rewrite c m g -> Rewrite c m g
anytdR r = setFailMsg "anytdR failed" $
           let go = r >+> anyR go
            in go
{-# INLINE anytdR #-}

-- | Apply a 'Rewrite' in a bottom-up manner, succeeding if any succeed.
anybuR :: (Walker c m g) => Rewrite c m g -> Rewrite c m g
anybuR r = setFailMsg "anybuR failed" $
           let go = anyR go >+> r
            in go
{-# INLINE anybuR #-}

-- | Apply a 'Rewrite' twice, in a top-down and bottom-up way, using one single tree traversal,
--   succeeding if any succeed.
anyduR :: (Walker c m g) => Rewrite c m g -> Rewrite c m g
anyduR r = setFailMsg "anyduR failed" $
           let go = r >+> anyR go >+> r
            in go
{-# INLINE anyduR #-}

-- | Apply a 'Rewrite' to the first 'Node' for which it can succeed, in a top-down traversal.
onetdR :: (Walker c m g) => Rewrite c m g -> Rewrite c m g
onetdR r = setFailMsg "onetdR failed" $
           let go = r <+ oneR go
            in go
{-# INLINE onetdR #-}

-- | Apply a 'Rewrite' to the first 'Node' for which it can succeed, in a bottom-up traversal.
onebuR :: (Walker c m g) => Rewrite c m g -> Rewrite c m g
onebuR r = setFailMsg "onetdR failed" $
           let go = oneR go <+ r
            in go
{-# INLINE onebuR #-}

-- | Attempt to apply a 'Rewrite' in a top-down manner, pruning at successful rewrites.
prunetdR :: (Walker c m g) => Rewrite c m g -> Rewrite c m g
prunetdR r = setFailMsg "prunetdR failed" $
             let go = r <+ anyR go
              in go
{-# INLINE prunetdR #-}

-- | A fixed-point traveral, starting with the innermost term.
innermostR :: (Walker c m g) => Rewrite c m g -> Rewrite c m g
innermostR r = setFailMsg "innermostR failed" $
               let go = anybuR (r >>> tryR go)
                in go
{-# INLINE innermostR #-}

-------------------------------------------------------------------------------

-- | A path from the root.
newtype AbsolutePath = AbsolutePath [Int]

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
--   Any user-defined combinators (typically 'childL' and congruence combinators) should update the 'AbsolutePath' using 'extendAbsPath'.
class PathContext c where
  -- | Find the current path.
  contextPath :: c -> AbsolutePath

-- | The simplest instance of 'PathContext' is 'AbsolutePath' itself.
instance PathContext AbsolutePath where
-- contextPath :: AbsolutePath -> AbsolutePath
   contextPath p = p
   {-# INLINE contextPath #-}

-- | Find the 'AbsolutePath' to the current 'Node'.
absPathT :: (PathContext c, Monad m) => Translate c m a AbsolutePath
absPathT = contextT >>^ contextPath
{-# INLINE absPathT #-}

-------------------------------------------------------------------------------

-- | A path is a route to descend the tree from an arbitrary 'Node'.
type Path = [Int]

-- | Convert an 'AbsolutePath' into a 'Path' starting at the root.
rootPath :: AbsolutePath -> Path
rootPath (AbsolutePath p) = reverse p
{-# INLINE rootPath #-}

--  Provided the first 'AbsolutePath' is a prefix of the second 'AbsolutePath',
--  computes the 'Path' from the end of the first to the end of the second.
rmPathPrefix :: AbsolutePath -> AbsolutePath -> Maybe Path
rmPathPrefix (AbsolutePath p1) (AbsolutePath p2) = do guard (p1 `isSuffixOf` p2)
                                                      return (drop (length p1) (reverse p2))
{-# INLINE rmPathPrefix #-}

--  Construct a 'Path' from the current 'Node' to the end of the given 'AbsolutePath', provided that 'AbsolutePath' passes through the current 'Node'.
abs2pathT :: (PathContext c, Monad m) => AbsolutePath -> Translate c m a Path
abs2pathT there = do here <- absPathT
                     maybe (fail "Absolute path does not pass through current node.") return (rmPathPrefix here there)
{-# INLINE abs2pathT #-}

-- | Find the 'Path's to every 'Node' that satisfies the predicate.
pathsToT :: (PathContext c, Walker c m g) => (g -> Bool) -> Translate c m g [Path]
pathsToT q = collectT (acceptR q "pathsToT" >>> absPathT) >>= mapM abs2pathT
{-# INLINE pathsToT #-}

-- | Find the 'Path' to the first 'Node' that satisfies the predicate (in a pre-order traversal).
onePathToT :: (PathContext c, Walker c m g) => (g -> Bool) -> Translate c m g Path
onePathToT q = setFailMsg "No matching nodes found." $
               onetdT (acceptR q "pathsToT" >>> absPathT) >>= abs2pathT
{-# INLINE onePathToT #-}

-- Note: The below definition is a hack.
-- Really we should have a more general "oneFocusT" that allows the Translate to depend on the integer, then this would be a trivial instantiation.
-- However, as oneFocusT (and family) are used A LOT, and this is just used once, I'm making do some code duplication for now.

-- | Find the 'Path' to the first descendent 'Node' that satisfies the predicate (in a pre-order traversal).
oneNonEmptyPathToT :: (PathContext c, Walker c m g) => (g -> Bool) -> Translate c m g Path
oneNonEmptyPathToT q = setFailMsg "No matching nodes found." $
                        do (cgs,_) <- multiLensT childrenL
                           let ncgs = zip [0..] cgs
                           constT $ catchesM $ map (\ (n,(c,g)) -> (n:) `liftM` apply (onePathToT q) c g) ncgs
{-# INLINE oneNonEmptyPathToT #-}

-- | Find the 'Path's to every 'Node' that satisfies the predicate, ignoring 'Node's below successes.
prunePathsToT :: (PathContext c, Walker c m g) => (g -> Bool) -> Translate c m g [Path]
prunePathsToT q = collectPruneT (acceptR q "pathsToT" >>> absPathT) >>= mapM abs2pathT
{-# INLINE prunePathsToT #-}


-- local function used by uniquePathToT and uniquePrunePathToT
requireUniquePath :: Monad m => Translate c m [Path] Path
requireUniquePath = contextfreeT $ \ ps -> case ps of
                                             []  -> fail "No matching nodes found."
                                             [p] -> return p
                                             _   -> fail $ "Ambiguous: " ++ show (length ps) ++ " matching nodes found."
{-# INLINE requireUniquePath #-}

-- | Find the 'Path' to the 'Node' that satisfies the predicate, failing if that does not uniquely identify a 'Node'.
uniquePathToT :: (PathContext c, Walker c m g) => (g -> Bool) -> Translate c m g Path
uniquePathToT q = pathsToT q >>> requireUniquePath
{-# INLINE uniquePathToT #-}

-- | Build a 'Path' to the 'Node' that satisfies the predicate, failing if that does not uniquely identify a 'Node' (ignoring 'Node's below successes).
uniquePrunePathToT :: (PathContext c, Walker c m g) => (g -> Bool) -> Translate c m g Path
uniquePrunePathToT q = prunePathsToT q >>> requireUniquePath
{-# INLINE uniquePrunePathToT #-}

-------------------------------------------------------------------------------

-- | Construct a 'Lens' by following a 'Path'.
pathL :: (Walker c m g) => Path -> Lens c m g g
pathL = andR . map childL
{-# INLINE pathL #-}

-- | Construct a 'Lens' that points to the last 'Node' at which the 'Path' can be followed.
exhaustPathL :: (Walker c m g) => Path -> Lens c m g g
exhaustPathL = foldr (\ n l -> tryR (childL n >>> l)) id
{-# INLINE exhaustPathL #-}

-- | Repeat as many iterations of the 'Path' as possible.
repeatPathL :: (Walker c m g) => Path -> Lens c m g g
repeatPathL p = let go = tryR (pathL p >>> go)
                 in go
{-# INLINE repeatPathL #-}

-- | Build a 'Lens' from the root to a point specified by an 'AbsolutePath'.
rootL :: (Walker c m g) => AbsolutePath -> Lens c m g g
rootL = pathL . rootPath
{-# INLINE rootL #-}

-------------------------------------------------------------------------------

-- | Apply a 'Rewrite' at a point specified by a 'Path'.
pathR :: (Walker c m g) => Path -> Rewrite c m g -> Rewrite c m g
pathR = focusR . pathL
{-# INLINE pathR #-}

-- | Apply a 'Translate' at a point specified by a 'Path'.
pathT :: (Walker c m g) => Path -> Translate c m g b -> Translate c m g b
pathT = focusT . pathL
{-# INLINE pathT #-}

-------------------------------------------------------------------------------

-- | Check if it is possible to construct a 'Lens' along this path from the current 'Node'.
testPathT :: (Walker c m g) => Path -> Translate c m g Bool
testPathT = testLensT . pathL
{-# INLINE testPathT #-}

-------------------------------------------------------------------------------

-- | Apply a 'Rewrite' to the largest node(s) that satisfy the predicate, requiring all to succeed.
allLargestR :: (Walker c m g) => Translate c m g Bool -> Rewrite c m g -> Rewrite c m g
allLargestR p r = prefixFailMsg "allLargestR failed: " $
                  let go = ifM p r (allR go)
                   in go
{-# INLINE allLargestR #-}

-- | Apply a 'Rewrite' to the largest node(s) that satisfy the predicate, succeeding if any succeed.
anyLargestR :: (Walker c m g) => Translate c m g Bool -> Rewrite c m g -> Rewrite c m g
anyLargestR p r = setFailMsg "anyLargestR failed" $
                  let go = ifM p r (anyR go)
                   in go
{-# INLINE anyLargestR #-}

-- | Apply a 'Rewrite' to the first node for which it can succeed among the largest node(s) that satisfy the predicate.
oneLargestR :: (Walker c m g) => Translate c m g Bool -> Rewrite c m g -> Rewrite c m g
oneLargestR p r = setFailMsg "oneLargestR failed" $
                  let go = ifM p r (oneR go)
                   in go
{-# INLINE oneLargestR #-}

-- | Apply a 'Translate' to the largest node(s) that satisfy the predicate, combining the results in a monoid.
allLargestT :: (Walker c m g, Monoid b) => Translate c m g Bool -> Translate c m g b -> Translate c m g b
allLargestT p t = prefixFailMsg "allLargestT failed: " $
                  let go = ifM p t (allT go)
                   in go
{-# INLINE allLargestT #-}

-- | Apply a 'Translate' to the first node for which it can succeed among the largest node(s) that satisfy the predicate.
oneLargestT :: (Walker c m g) => Translate c m g Bool -> Translate c m g b -> Translate c m g b
oneLargestT p t = setFailMsg "oneLargestT failed" $
                  let go = ifM p t (oneT go)
                   in go
{-# INLINE oneLargestT #-}

-- | Test if the type of the current ('Generic') 'Node' matches the type of the argument.
--   Note that the argument /value/ is never inspected, it is merely a proxy for a type argument.
summandIsTypeT :: forall c m a g. (MonadCatch m, Injection a g) => a -> Translate c m g Bool
summandIsTypeT _ = arr (isJust . (retract :: (g -> Maybe a)))
{-# INLINE summandIsTypeT #-}

-------------------------------------------------------------------------------
