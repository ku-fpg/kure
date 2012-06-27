{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}

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
-- There is no mechanism for \"ascending\" the tree.

module Language.KURE.Walker
        ( -- * Nodes
          Node(..)
        , numChildrenT
        , hasChild
        , hasChildT

          -- * Tree Walkers
        , Walker(..)

        -- * Rewrite Traversals
        , childR
        , alltdR
        , anytdR
        , allbuR
        , anybuR
        , allduR
        , anyduR
        , prunetdR
        , innermostR

        -- * Translate Traversals
        , childT
        , foldtdT
        , foldbuT
        , prunetdT
        , crushtdT
        , crushbuT
        , collectT
        , collectPruneT

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
        , prunePathsToT
        , uniquePathToT
        , uniquePrunePathToT
        , firstPathToT

        -- * Using Paths
        -- ** Building Lenses from Paths
        , pathL
        , exhaustPathL
        , repeatPathL
        , rootL

        -- ** Applying transformations at the end of Paths
        ,  pathR
        ,  pathT

        -- ** Testing Paths
        ,  testPathT


) where

import Prelude hiding (id)

import Data.Monoid
import Data.List
import Control.Monad
import Control.Arrow
import Control.Category hiding ((.))

import Language.KURE.Combinators
import Language.KURE.Translate
import Language.KURE.Injection

------------------------------------------------------------------------------------------


-- | A 'Node' is any node in the tree that you wish to be able to traverse.

class (Injection a (Generic a), Generic a ~ Generic (Generic a)) => Node a where

  -- | 'Generic' is a sum of all the types of the sub-nodes, transitively, of @a@.
  -- We use @Generic a ~ a@ to signify that something is its own Generic.
  -- Simple expression types might be their own sole 'Generic', more complex examples
  -- will have a new datatype for the 'Generic', which will also be an instance of class 'Node'.
  type Generic a :: *

  -- | Count the number of immediate child 'Node's.
  numChildren :: a -> Int

-- | Lifted version of 'numChildren'.
numChildrenT :: (Monad m, Node a) => Translate c m a Int
numChildrenT = arr numChildren

-- | Check if a 'Node' has a child of the specified index.
hasChild :: Node a => Int -> a -> Bool
hasChild n a = (0 <= n) && (n < numChildren a)

-- | Lifted version of 'hasChild'.
hasChildT :: (Monad m, Node a) => Int -> Translate c m a Bool
hasChildT = arr . hasChild

-------------------------------------------------------------------------------

-- | 'Walker' captures the ability to walk over a tree of 'Node's,
--   using a specific context @c@ and a 'MonadCatch' @m@.
--
--   Minimal complete definition: 'childL'.
--
--   Default instances are provided for 'allT', 'allR' and 'anyR', but they may be overridden for efficiency.
--   For small numbers of interesting children this will not be an issue, but for a large number, say
--   for a list of children, it may be.

class (MonadCatch m, Node a) => Walker c m a where

  -- | Construct a 'Lens' to the n-th child 'Node'.
  childL :: Int -> Lens c m a (Generic a)

  -- | Apply a 'Generic' 'Translate' to all immediate child 'Node's, succeeding if they all succeed.
  --   The results are combined in a 'Monoid'.
  allT :: Monoid b => Translate c m (Generic a) b -> Translate c m a b
  allT t = do n <- arr numChildren
              mconcat [ childT i t | i <- [0..(n-1)] ]

  -- | Apply a 'Generic' 'Rewrite' to all interesting children of this 'Node', succeeding if they all succeed.
  allR :: Rewrite c m (Generic a) -> Rewrite c m a
  allR r = do n <- arr numChildren
              andR [ childR i r | i <- [0..(n-1)] ]

  -- | Apply 'Generic' 'Rewrite' to all interesting children of this 'Node', suceeding if any succeed.
  anyR :: Rewrite c m (Generic a) -> Rewrite c m a
  anyR r = do n <- arr numChildren
              orR [ childR i r | i <- [0..(n-1)] ]

-- | Apply a 'Translate' to a specific child 'Node'.
childT :: Walker c m a => Int -> Translate c m (Generic a) b -> Translate c m a b
childT n = focusT (childL n)

-- | Apply a 'Rewrite' to a specific child 'Node'.
childR :: Walker c m a => Int -> Rewrite c m (Generic a) -> Rewrite c m a
childR n = focusR (childL n)

-------------------------------------------------------------------------------

-- | Fold a tree in a top-down manner, using a single 'Translate' for each 'Node'.
foldtdT :: (Walker c m a, Monoid b, a ~ Generic a) => Translate c m (Generic a) b -> Translate c m (Generic a) b
foldtdT t = t `mappend` allT (foldtdT t)

-- | Fold a tree in a bottom-up manner, using a single 'Translate' for each 'Node'.
foldbuT :: (Walker c m a, Monoid b, a ~ Generic a) => Translate c m (Generic a) b -> Translate c m (Generic a) b
foldbuT t = allT (foldbuT t) `mappend` t

-- | Attempt to apply a 'Translate' in a top-down manner, prunning at successes.
prunetdT :: (Walker c m a, Monoid b, a ~ Generic a) => Translate c m (Generic a) b -> Translate c m (Generic a) b
prunetdT t = t <+ allT (prunetdT t)

-- | An always successful top-down fold, replacing failures with 'mempty'.
crushtdT :: (Walker c m a, Monoid b, a ~ Generic a) => Translate c m (Generic a) b -> Translate c m (Generic a) b
crushtdT t = foldtdT (mtryM t)

-- | An always successful bottom-up fold, replacing failures with 'mempty'.
crushbuT :: (Walker c m a, Monoid b, a ~ Generic a) => Translate c m (Generic a) b -> Translate c m (Generic a) b
crushbuT t = foldbuT (mtryM t)

-- | An always successful traversal that collects the results of all successful applications of a 'Translate' in a list.
collectT :: (Walker c m a, a ~ Generic a) => Translate c m (Generic a) b -> Translate c m (Generic a) [b]
collectT t = crushtdT (t >>^ (\ b -> [b]))

-- | Like 'collectT', but does not traverse below successes.
collectPruneT :: (Walker c m a, a ~ Generic a) => Translate c m (Generic a) b -> Translate c m (Generic a) [b]
collectPruneT t = prunetdT (t >>^ (\ b -> [b]))

-------------------------------------------------------------------------------

-- | Apply a 'Rewrite' in a top-down manner, succeeding if they all succeed.
alltdR :: (Walker c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
alltdR r = r >>> allR (alltdR r)

-- | Apply a 'Rewrite' in a top-down manner, succeeding if any succeed.
anytdR :: (Walker c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
anytdR r = r >+> anyR (anytdR r)

-- | Apply a 'Rewrite' in a bottom-up manner, succeeding if they all succeed.
allbuR :: (Walker c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
allbuR r = allR (allbuR r) >>> r

-- | Apply a 'Rewrite' in a bottom-up manner, succeeding if any succeed.
anybuR :: (Walker c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
anybuR r = anyR (anybuR r) >+> r

-- | Apply a 'Rewrite' twice, in a top-down and bottom-up way, using one single tree traversal,
--   succeeding if they all succeed.
allduR :: (Walker c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
allduR r = r >>> allR (allduR r) >>> r

-- | Apply a 'Rewrite' twice, in a top-down and bottom-up way, using one single tree traversal,
--   succeeding if any succeed.
anyduR :: (Walker c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
anyduR r = r >+> anyR (anyduR r) >+> r

-- | Attempt to apply a 'Rewrite' in a top-down manner, prunning at successful rewrites.
prunetdR :: (Walker c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
prunetdR r = r <+ anyR (prunetdR r)

-- | A fixed-point traveral, starting with the innermost term.
innermostR :: (Walker c m a, Generic a ~ a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
innermostR r = anybuR (r >>> tryR (innermostR r))

-------------------------------------------------------------------------------

-- | A path from the root.
newtype AbsolutePath = AbsolutePath [Int]

instance Show AbsolutePath where
  show (AbsolutePath p) = show (reverse p)

-- | The (empty) 'AbsolutePath' to the root.
rootAbsPath :: AbsolutePath
rootAbsPath = AbsolutePath []

-- | Extend an 'AbsolutePath' by one descent.
extendAbsPath :: Int -> AbsolutePath -> AbsolutePath
extendAbsPath n (AbsolutePath ns) = AbsolutePath (n:ns)

-- | Contexts that are instances of 'PathContext' contain the current 'AbsolutePath'.
--   Any user-defined combinators (typically 'childL' and congruence combinators) should update the 'AbsolutePath' using 'extendAbsPath'.
class PathContext c where
  -- | Find the current path.
  contextPath :: c -> AbsolutePath

-- | The simplest instance of 'PathContext' is 'AbsolutePath' itself.
instance PathContext AbsolutePath where
-- contextPath :: AbsolutePath -> AbsolutePath
   contextPath p = p

-- | Find the 'AbsolutePath' to the current 'Node'.
absPathT :: (PathContext c, Monad m) => Translate c m a AbsolutePath
absPathT = contextT >>^ contextPath

-------------------------------------------------------------------------------

-- | A path is a route to descend the tree from an arbitrary 'Node'.
type Path = [Int]

-- | Convert an 'AbsolutePath' into a 'Path' starting at the root.
rootPath :: AbsolutePath -> Path
rootPath (AbsolutePath p) = reverse p

--  Provided the first 'AbsolutePath' is a prefix of the second 'AbsolutePath',
--  computes the 'Path' from the end of the first to the end of the second.
rmPathPrefix :: AbsolutePath -> AbsolutePath -> Maybe Path
rmPathPrefix (AbsolutePath p1) (AbsolutePath p2) = do guard (p1 `isSuffixOf` p2)
                                                      return (drop (length p1) (reverse p2))

--  Construct a 'Path' from the current 'Node' to the end of the given 'AbsolutePath', provided that 'AbsolutePath' passes through the current 'Node'.
abs2pathT :: (PathContext c, Monad m) => AbsolutePath -> Translate c m a Path
abs2pathT there = do here <- absPathT
                     maybe (fail "Absolute path does not pass through current node.") return (rmPathPrefix here there)

-- | Find the 'Path's to every descendent 'Node' that satisfies the predicate.
pathsToT :: (PathContext c, Walker c m a, a ~ Generic a) => (Generic a -> Bool) -> Translate c m (Generic a) [Path]
pathsToT q = collectT (acceptR q >>> absPathT) >>= mapM abs2pathT

-- | Find the 'Path's to every descendent 'Node' that satisfies the predicate, ignoring 'Node's below successes.
prunePathsToT :: (PathContext c, Walker c m a, a ~ Generic a) => (Generic a -> Bool) -> Translate c m (Generic a) [Path]
prunePathsToT q = collectPruneT (acceptR q >>> absPathT) >>= mapM abs2pathT

-- local function used by uniquePathToT and uniquePrunePathToT
requireUniquePath :: Monad m => Translate c m [Path] Path
requireUniquePath = contextfreeT $ \ ps -> case ps of
                                             []  -> fail "No matching nodes found."
                                             [p] -> return p
                                             _   -> fail $ "Ambiguous: " ++ show (length ps) ++ " matching nodes found."

-- | Find the 'Path' to the descendent 'Node' that satisfies the predicate, failing if that does not uniquely identify a 'Node'.
uniquePathToT :: (PathContext c, Walker c m a, a ~ Generic a) => (Generic a -> Bool) -> Translate c m (Generic a) Path
uniquePathToT q = pathsToT q >>> requireUniquePath

-- | Build a 'Path' to the descendent 'Node' that satisfies the predicate, failing if that does not uniquely identify a 'Node' (ignoring 'Node's below successes).
uniquePrunePathToT :: (PathContext c, Walker c m a, a ~ Generic a) => (Generic a -> Bool) -> Translate c m (Generic a) Path
uniquePrunePathToT q = prunePathsToT q >>> requireUniquePath

-- | Build a 'Path' to the first descendent 'Node' that satisfies the predicate (in a pre-order traversal).
firstPathToT :: (PathContext c, Walker c m a, a ~ Generic a) => (Generic a -> Bool) -> Translate c m (Generic a) Path
firstPathToT q = withPatFailMsg "No matching nodes found." $ do (pa : _) <- pathsToT q
                                                                return pa

-------------------------------------------------------------------------------

-- | Construct a 'Lens' by following a 'Path'.
pathL :: (Walker c m a, a ~ Generic a) => Path -> Lens c m (Generic a) (Generic a)
pathL = andR . map childL

-- | Construct a 'Lens' that points to the last 'Node' at which the 'Path' can be followed.
exhaustPathL :: (Walker c m a, a ~ Generic a) => Path -> Lens c m (Generic a) (Generic a)
exhaustPathL = foldr (\ n l -> tryR (childL n >>> l)) id

-- | Repeat as many iterations of the 'Path' as possible.
repeatPathL :: (Walker c m a, a ~ Generic a) => Path -> Lens c m (Generic a) (Generic a)
repeatPathL p = tryR (pathL p >>> repeatPathL p)

-- | Build a 'Lens' from the root to a point specified by an 'AbsolutePath'.
rootL :: (Walker c m a, a ~ Generic a) => AbsolutePath -> Lens c m (Generic a) (Generic a)
rootL = pathL . rootPath

-------------------------------------------------------------------------------

-- | Apply a 'Rewrite' at a point specified by a 'Path'.
pathR :: (Walker c m a, a ~ Generic a) => Path -> Rewrite c m (Generic a) -> Rewrite c m (Generic a)
pathR = focusR . pathL

-- | Apply a 'Translate' at a point specified by a 'Path'.
pathT :: (Walker c m a, a ~ Generic a) => Path -> Translate c m (Generic a) b -> Translate c m (Generic a) b
pathT = focusT . pathL

-------------------------------------------------------------------------------

-- | Check if it is possible to construct a 'Lens' along this path from the current 'Node'.
testPathT :: (Walker c m a, a ~ Generic a) => Path -> Translate c m a Bool
testPathT = testLensT . pathL

-------------------------------------------------------------------------------
