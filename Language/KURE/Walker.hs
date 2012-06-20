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
        ( -- * Traversal Classes
          Term(..)
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
        , ascendAbsPath
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

        -- ** Applying transformations at the end of 'Path's
        ,  pathR
        ,  pathT


) where

import Data.Monoid
import Data.List
import Control.Monad
import Control.Arrow

import Language.KURE.Combinators
import Language.KURE.Translate
import Language.KURE.Injection

------------------------------------------------------------------------------------------


-- | A 'Term' is any node in the tree that you wish to be able to traverse.

class (Injection a (Generic a), Generic a ~ Generic (Generic a)) => Term a where

  -- | 'Generic' is a sum of all the interesting sub-types, transitively, of @a@.
  -- We use @Generic a ~ a@ to signify that something is its own Generic.
  -- Simple expression types might be their own sole 'Generic', more complex examples
  -- will have a new datatype for the 'Generic', which will also be an instance of class 'Term'.
  type Generic a :: *

  -- | Count the number of interesting children.
  numChildren :: a -> Int

-------------------------------------------------------------------------------

-- | 'Walker' captures the ability to walk over a 'Term' applying 'Rewrite's,
--   using a specific context @c@ and a 'MonadPlus' @m@.
--
--   Minimal complete definition: 'childL'.
--
--   Default instances are provided for 'allT', 'allR' and 'anyR', but they may be overridden for efficiency.
--   For small numbers of interesting children this will not be an issue, but for a large number, say
--   for a list of children, it may be.

class (MonadPlus m, Term a) => Walker c m a where

  -- | Construct a 'Lens' pointing at the n-th interesting child of this node.
  childL :: Int -> Lens c m a (Generic a)

  -- | Apply a 'Generic' 'Translate' to all interesting children of this node, succeeding if they all succeed.
  --   The results are combined in a 'Monoid'.
  allT :: Monoid b => Translate c m (Generic a) b -> Translate c m a b
  allT t = do n <- arr numChildren
              mconcat [ childT i t | i <- [0..(n-1)] ]

  -- | Apply a 'Generic' 'Rewrite' to all interesting children of this node, succeeding if they all succeed.
  allR :: Rewrite c m (Generic a) -> Rewrite c m a
  allR r = do n <- arr numChildren
              andR [ childR i r | i <- [0..(n-1)] ]

  -- | Apply 'Generic' 'Rewrite' to all interesting children of this node, suceeding if any succeed.
  anyR :: Rewrite c m (Generic a) -> Rewrite c m a
  anyR r = do n <- arr numChildren
              orR [ childR i r | i <- [0..(n-1)] ]

-- | Apply a 'Translate' to a specific child.
childT :: Walker c m a => Int -> Translate c m (Generic a) b -> Translate c m a b
childT n = focusT (childL n)

-- | Apply a 'Rewrite' to a specific child.
childR :: Walker c m a => Int -> Rewrite c m (Generic a) -> Rewrite c m a
childR n = focusR (childL n)

-------------------------------------------------------------------------------

-- | Fold a tree in a top-down manner, using a single 'Translate' for each node.
foldtdT :: (Walker c m a, Monoid b, a ~ Generic a) => Translate c m (Generic a) b -> Translate c m (Generic a) b
foldtdT t = t `mappend` allT (foldtdT t)

-- | Fold a tree in a bottom-up manner, using a single 'Translate' for each node.
foldbuT :: (Walker c m a, Monoid b, a ~ Generic a) => Translate c m (Generic a) b -> Translate c m (Generic a) b
foldbuT t = allT (foldbuT t) `mappend` t

-- | Attempt to apply a 'Translate' in a top-down manner, prunning at successes.
prunetdT :: (Walker c m a, Monoid b, a ~ Generic a) => Translate c m (Generic a) b -> Translate c m (Generic a) b
prunetdT t = t <+> allT (prunetdT t)

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
prunetdR r = r <+> anyR (prunetdR r)

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
extendAbsPath n (AbsolutePath p) = AbsolutePath (n : p)

-- | Ascend one step up an 'AbsolutePath', revealing the number of the descent neccassary to restore the path.
ascendAbsPath :: AbsolutePath -> Maybe (AbsolutePath , Int)
ascendAbsPath (AbsolutePath [])     = Nothing
ascendAbsPath (AbsolutePath (n:ns)) = Just (AbsolutePath ns , n)

-- | Contexts that are instances of 'PathContext' contain the current 'AbsolutePath'.
--   Any user-defined combinators (typically 'childL' and congruence combinators) should update the 'AbsolutePath' using 'extendAbsolutePath'.
class PathContext c where
  -- | Find the current path.
  contextPath :: c -> AbsolutePath

-- | The simplest instance of 'PathContext' is 'AbsolutePath' itself.
instance PathContext AbsolutePath where
-- contextPath :: AbsolutePath -> AbsolutePath
   contextPath p = p

-- | Find the 'AbsolutePath' to the current node.
absPathT :: (PathContext c, Monad m) => Translate c m a AbsolutePath
absPathT = contextT >>^ contextPath

-------------------------------------------------------------------------------

-- | A path is a route to descend the tree from an arbitrary node.
type Path = [Int]

-- | Convert an 'AbsolutePath' into a 'Path' starting at the root.
rootPath :: AbsolutePath -> Path
rootPath (AbsolutePath p) = reverse p

--  Provided the first 'AbsolutePath' is a prefix of the second 'AbsolutePath',
--  computes the 'Path' from the end of the first to the end of the second.
rmPathPrefix :: AbsolutePath -> AbsolutePath -> Maybe Path
rmPathPrefix (AbsolutePath p1) (AbsolutePath p2) = do guard (p1 `isSuffixOf` p2)
                                                      return (drop (length p1) (reverse p2))

--  Construct a 'Path' from the current node to the end of the given 'AbsolutePath', provided that 'AbsolutePath' passes through the current node.
abs2pathT :: (PathContext c, Monad m) => AbsolutePath -> Translate c m a Path
abs2pathT there = do here <- absPathT
                     maybe (fail "Absolute path does not pass through current node.") return (rmPathPrefix here there)

-- | Find the 'Path's to every descendent node that satisfies the predicate.
pathsToT :: (PathContext c, Walker c m a, a ~ Generic a) => (Generic a -> Bool) -> Translate c m (Generic a) [Path]
pathsToT p = collectT (acceptR p >>> absPathT) >>= mapM abs2pathT

-- | Find the 'Path's to every descendent node that satisfies the predicate, ignoring nodes below successes.
prunePathsToT :: (PathContext c, Walker c m a, a ~ Generic a) => (Generic a -> Bool) -> Translate c m (Generic a) [Path]
prunePathsToT p = collectPruneT (acceptR p >>> absPathT) >>= mapM abs2pathT

-- | Find the 'Path' to the descendent node that satisfies the predicate, failing if that does not uniquely identify a node.
uniquePathToT :: (PathContext c, Walker c m a, a ~ Generic a) => (Generic a -> Bool) -> Translate c m (Generic a) Path
uniquePathToT p = do [pa] <- pathsToT p
                     return pa

-- | Build a 'Path' to the descendent node that satisfies the predicate, failing if that does not uniquely identify a node (ignoring nodes below successes).
uniquePrunePathToT :: (PathContext c, Walker c m a, a ~ Generic a) => (Generic a -> Bool) -> Translate c m (Generic a) Path
uniquePrunePathToT p = do [pa] <- prunePathsToT p
                          return pa

-- | Build a 'Path' to the first descendent node that satisfies the predicate (in a pre-order traversal).
firstPathToT :: (PathContext c, Walker c m a, a ~ Generic a) => (Generic a -> Bool) -> Translate c m (Generic a) Path
firstPathToT p = do (pa : _) <- pathsToT p
                    return pa

-------------------------------------------------------------------------------

-- | Construct a 'Lens' by following a 'Path'.
pathL :: (Walker c m a, a ~ Generic a) => Path -> Lens c m (Generic a) (Generic a)
pathL = andR . map childL

-- | Construct a 'Lens' that points to the last node at which the 'Path' can be followed.
exhaustPathL :: (Walker c m a, a ~ Generic a) => Path -> Lens c m (Generic a) (Generic a)
exhaustPathL = foldr (\ n l -> tryR (childL n >>> l)) idR

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
