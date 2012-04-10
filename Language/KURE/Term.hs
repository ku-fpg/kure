{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
-- |
-- Module: Language.KURE.Translate
-- Copyright: (c) 2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- This module contains the combinators that allow us to traverse an expression tree.

module Language.KURE.Term
-- module Term
--                      
--         ( extractR
--         , promoteR
--         , extractT
--         , promoteT
--         , topdownR
--         , bottomupR
--         , alltdR
--         , downupR
--         , innermostR
--         , foldT
-- )
 where

import Language.KURE.Translate
-- import Translate

import Data.Monoid
import Data.Pointed
import Control.Applicative

------------------------------------------------------------------------------------------

-- | 'Term's are things that syntax are built from.
class (Generic a ~ Generic (Generic a)) => Term a where
  
  -- | 'Generic' is a sum of all the interesting sub-types, transitively, of @a@.
  -- We use @Generic a ~ a@ to signify that something is its own Generic.
  -- Simple expression types might be their own sole 'Generic', more complex examples
  -- will have a new datatype for the 'Generic', which will also be an instance of class 'Term'.
  type Generic a
  
  -- | 'inject' injects a term into a 'Generic'. 
  inject :: a -> Generic a
  
  -- | 'retract' attempts to extract a term from within a 'Generic'. 
  retract :: Generic a -> Maybe a

  -- | 'allR' applies 'Generic' rewrites to all the interesting children of this node.
  allR :: Applicative m => Rewrite c m (Generic a) -> Rewrite c m a
  
  -- | 'crushT' applies a 'Generic' Translate to a common, 'Monoid'al result, to all the interesting children of this node.
  crushT :: (Applicative m, Monoid b) => Translate c m (Generic a) b -> Translate c m a b
  

-- | 'Walker' captures how we walk over an expression in a context, using a monad m. 
-- class (Applicative m, Term a) => Walker c m a where

--   -- | 'allR' applies 'Generic' rewrites to all the interesting children of this node.
--   allR :: Rewrite c m (Generic a) -> Rewrite c m a

--   -- | 'crushT' applies a 'Generic' Translate to a common, 'Monoid'al result, to all the interesting children of this node.
--   crushT :: Monoid b => Translate c m (Generic a) b -> Translate c m a b
  
--------------------------------------------------------------------------------

-- | 'extractT' converts a 'Translate' taking a 'Generic' into a translate over a specific expression type.
extractT :: Term a => Translate c m (Generic a) b -> Translate c m a b
extractT t = translate $ \ c -> apply t c . inject

-- | 'promoteT' promotes a 'Translate' into a 'Generic' 'Translate'; other types inside Generic cause failure.
promoteT  :: (Alternative m, Term a) => Translate c m a b -> Translate c m (Generic a) b
promoteT t = translate $ \ c -> maybe empty (apply t c) . retract

-- | 'extractR' converts a 'Rewrite' over a 'Generic' into a rewrite over a specific expression type.
extractR :: (Pointed m, Alternative m, Monad m, Term a) => Rewrite c m (Generic a) -> Rewrite c m a
extractR r =  extractT r >-> liftT retract >-> fromJustT
  
-- | 'promoteR' promotes a 'Rewrite' into a 'Generic' 'Rewrite'; other types inside Generic cause failure.
--   'try' can be used to convert a failure-by-default promoteR into a 'id-by-default' promotion.
promoteR  :: (Alternative m, Term a) => Rewrite c m a -> Rewrite c m (Generic a)
promoteR = liftA inject . promoteT

-------------------------------------------------------------------------------

-- | 'Walker' is a constraint synonym that collects together the constraints common to all of the following traversal combinators. 
type Walker m a = (Pointed m, Applicative m, Term a, a ~ Generic a)

-- | apply a 'Rewrite' in a top down manner.
topdownR :: (Monad m, Walker m a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
topdownR r = r >-> allR (topdownR r)

-- | apply a 'Rewrite' in a bottom up manner.
bottomupR :: (Monad m, Walker m a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
bottomupR r = allR (bottomupR r) >-> r

-- | apply a 'Rewrite' in a top down manner, prunning at successful rewrites.
alltdR :: (Alternative m, Walker m a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
alltdR r = r <+ allR (alltdR r)

-- | apply a 'Rewrite' twice, in a topdown and bottom up way, using one single tree traversal.
downupR :: (Monad m, Walker m a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
downupR r = r >-> allR (downupR r) >-> r

-- | a fixed point traveral, starting with the innermost term.
innermostR :: (Alternative m, Monad m, Walker m a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
innermostR r = bottomupR (tryR (r >-> innermostR r))

-- | fold a tree using a single 'Translate' for each node.
foldT :: (Walker m a, Monoid b) => Translate c m (Generic a) b -> Translate c m (Generic a) b
foldT t = concatT [ t, crushT (foldT t) ]

-------------------------------------------------------------------------------
