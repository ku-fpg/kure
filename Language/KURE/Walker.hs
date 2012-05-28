{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}

-- |
-- Module: Language.KURE.Walker
-- Copyright: (c) 2006-2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: alpha
-- Portability: ghc
--
-- This module contains combinators that allow us to traverse an expression tree.

module Language.KURE.Walker
        (
          Term
        , Generic
        , numChildren

-- |    selecting one child node
        , Walker
        , childL

-- |    'Rewrite's on children or descendents
        , childR
        , allR
        , anyR
        , alltdR
        , anytdR
        , allbuR
        , anybuR
        , allduR
        , anyduR
        , tdpruneR
        , innermostR

-- |    'Translate's on children or descendents
        , childT
        , allT
        , foldtdT
        , foldbuT
        , tdpruneT
        , crushtdT
        , crushbuT

-- |    Building 'Lens's
        , Path
        , pathL
        , exhaustPathL
        , repeatPathL
) where

import Data.Monoid
import Control.Monad
import Control.Arrow

import Language.KURE.Combinators
import Language.KURE.Translate
import Language.KURE.Injection

------------------------------------------------------------------------------------------

-- | 'Term's are things that syntax are built from.
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
--   using a specific context @c@ and a 'Monad' @m@.
--   Default instances are provided for 'allT', 'allR' and 'anyR', but they may be overridden for efficiency.
--   For small number of interesting children this will not be an issue, but for a large number, say,
--   for a list of children, it may be.
class (MonadPlus m, Term a) => Walker c m a where

  -- | 'childL' constructs a 'Lens' pointing at the n-th interesting child of this node.
  childL :: Int -> Lens c m a (Generic a)

  -- | 'allT' applies a 'Generic' 'Translate' to all interesting children of this node, succeeding if they all succeed.
  --   The results are combined in a 'Monoid'.
  allT :: Monoid b => Translate c m (Generic a) b -> Translate c m a b
  allT t = do n <- arr numChildren
              mconcatM [ childT i t | i <- [0..(n-1)] ]

  -- | 'allR' applies a 'Generic' 'Rewrite' to all interesting children of this node, succeeding if they all succeed.
  allR :: Rewrite c m (Generic a) -> Rewrite c m a
  allR r = do n <- arr numChildren
              andR [ childR i r | i <- [0..(n-1)] ]

  -- | 'anyR' applies 'Generic' 'Rewrite' to all interesting children of this node, suceeding if any succeed.
  anyR :: Rewrite c m (Generic a) -> Rewrite c m a
  anyR r = do n <- arr numChildren
              orR [ childR i r | i <- [0..(n-1)] ]

-- | apply a 'Translate' to a specific child.
childT :: Walker c m a => Int -> Translate c m (Generic a) b -> Translate c m a b
childT n = focusT (childL n)

-- | apply a 'Rewrite' to a specific child.
childR :: Walker c m a => Int -> Rewrite c m (Generic a) -> Rewrite c m a
childR n = focusR (childL n)

-------------------------------------------------------------------------------

-- | fold a tree in a top-down manner, using a single 'Translate' for each node.
foldtdT :: (Walker c m a, Monoid b, a ~ Generic a) => Translate c m (Generic a) b -> Translate c m (Generic a) b
foldtdT t = mconcatM [ t, allT (foldtdT t) ]

-- | fold a tree in a bottom-up manner, using a single 'Translate' for each node.
foldbuT :: (Walker c m a, Monoid b, a ~ Generic a) => Translate c m (Generic a) b -> Translate c m (Generic a) b
foldbuT t = mconcatM [ allT (foldbuT t), t ]

-- | attempt to apply a 'Translate' in a top-down manner, prunning at successes.
tdpruneT :: (Walker c m a, Monoid b, a ~ Generic a) => Translate c m (Generic a) b -> Translate c m (Generic a) b
tdpruneT t = t <+> allT (tdpruneT t)

-- | an always successful top-down fold, replacing failures with 'mempty'.
crushtdT :: (Walker c m a, Monoid b, a ~ Generic a) => Translate c m (Generic a) b -> Translate c m (Generic a) b
crushtdT t = foldtdT (mtryM t)

-- | -- | an always successful bottom-up fold, replacing failures with 'mempty'.
crushbuT :: (Walker c m a, Monoid b, a ~ Generic a) => Translate c m (Generic a) b -> Translate c m (Generic a) b
crushbuT t = foldbuT (mtryM t)

-------------------------------------------------------------------------------

-- | apply a 'Rewrite' in a top-down manner, succeeding if they all succeed.
alltdR :: (Walker c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
alltdR r = r >>> allR (alltdR r)

-- | apply a 'Rewrite' in a top-down manner, succeeding if any succeed.
anytdR :: (Walker c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
anytdR r = r >+> anyR (anytdR r)

-- | apply a 'Rewrite' in a bottom-up manner, succeeding if they all succeed.
allbuR :: (Walker c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
allbuR r = allR (allbuR r) >>> r

-- | apply a 'Rewrite' in a bottom-up manner, succeeding if any succeed.
anybuR :: (Walker c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
anybuR r = anyR (anybuR r) >+> r

-- | apply a 'Rewrite' twice, in a top-down and bottom-up way, using one single tree traversal,
--   succeeding if they all succeed.
allduR :: (Walker c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
allduR r = r >>> allR (allduR r) >>> r

-- | apply a 'Rewrite' twice, in a top-down and bottom-up way, using one single tree traversal,
--   succeeding if any succeed.
anyduR :: (Walker c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
anyduR r = r >+> anyR (anyduR r) >+> r

-- | attempt to apply a 'Rewrite' in a top-down manner, prunning at successful rewrites.
tdpruneR :: (Walker c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
tdpruneR r = r <+> anyR (tdpruneR r)

-- | a fixed-point traveral, starting with the innermost term.
innermostR :: (Walker c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
innermostR r = allbuR (tryR (r >>> innermostR r))

-------------------------------------------------------------------------------

-- | a 'Path' is a list of 'Int's, where each 'Int' specifies which interesting child to descend to at each step.
type Path = [Int]

-- | construct a 'Lens' by following a 'Path'.
pathL :: (Walker c m a, a ~ Generic a) => Path -> Lens c m (Generic a) (Generic a)
pathL = sequenceL . map childL

-- | construct a 'Lens' that points to the last node at which the 'Path' can be followed.
exhaustPathL :: (Walker c m a, a ~ Generic a) => Path -> Lens c m (Generic a) (Generic a)
exhaustPathL []     = idL
exhaustPathL (n:ns) = tryL (childL n `composeL` exhaustPathL ns)

-- | repeat as many iterations of the 'Path' as possible.
repeatPathL :: (Walker c m a, a ~ Generic a) => Path -> Lens c m (Generic a) (Generic a)
repeatPathL p = tryL (pathL p `composeL` repeatPathL p)

-------------------------------------------------------------------------------
