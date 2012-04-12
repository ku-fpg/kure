{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, KindSignatures, ConstraintKinds, FlexibleInstances #-}
-- |
-- Module: Language.KURE.Translate
-- Copyright: (c) 2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- This module contains combinators that allow us to traverse an expression tree.

module Language.KURE.Term      
        ( Injection, inject, retract  
        , Term, Generic 
        , fromMaybeA        
        , maybeA  
        , retractA  
        , retractWithA  
        , extractR
        , promoteR
        , extractT
        , promoteT  
        , extractL  
        , promoteL
        , Walker, crushT, allR, chooseL  
        , WalkerR                  
        , topdownR
        , bottomupR
        , alltdR
        , downupR
        , innermostR
        , WalkerT  
        , topdownT
        , bottomupT
        , pathL  
) where

import Language.KURE.Translate

import Data.Monoid
import Control.Applicative

------------------------------------------------------------------------------------------

-- | A class of injective functions from @a@ to @b@, and their retractions.
--   The following law is expected to hold:  retract (inject a) == Just a
class Injection a b where
  inject  :: a -> b
  retract :: b -> Maybe a

-- | There is an identity injection for all types.
instance Injection a a where
  inject  = id
  retract = Just

-- | 'Term's are things that syntax are built from.
class (Injection a (Generic a), Generic a ~ Generic (Generic a)) => Term a where
  -- | 'Generic' is a sum of all the interesting sub-types, transitively, of @a@.
  -- We use @Generic a ~ a@ to signify that something is its own Generic.
  -- Simple expression types might be their own sole 'Generic', more complex examples
  -- will have a new datatype for the 'Generic', which will also be an instance of class 'Term'.
  type Generic a :: *
  
--------------------------------------------------------------------------------

-- | converts a 'Maybe' into an 'Alternative'; mapping 'Nothing' to 'empty' and 'Just' to 'pure'.
fromMaybeA :: Alternative m => Maybe a -> m a  
fromMaybeA = maybe empty pure

-- | maps a function over a 'Maybe' before converting it to an 'Alternative'.
maybeA :: Alternative m => (a -> b) -> Maybe a -> m b
maybeA f = fromMaybeA . liftA f 
           
-- | attempts to extract an @a@ from a @Generic a@.
retractA :: (Alternative m, Term a) => Generic a -> m a
retractA = fromMaybeA . retract

-- | attempts to extract an @a@ from a @Generic a@, and then maps a function over it.
--   can be useful when defining 'chooseL' instances.
retractWithA :: (Alternative m, Term a) => (a -> b) -> Generic a -> m b
retractWithA f = maybeA f . retract

--------------------------------------------------------------------------------

-- | 'extractT' converts a 'Translate' taking a 'Generic' into a translate over a specific expression type.
extractT :: Term a => Translate c m (Generic a) b -> Translate c m a b
extractT t = translate $ \ c -> apply t c . inject

-- | 'promoteT' promotes a 'Translate' into a 'Generic' 'Translate'; other types inside Generic cause failure.
promoteT  :: (Alternative m, Term a) => Translate c m a b -> Translate c m (Generic a) b
promoteT t = translate $ \ c -> maybe empty (apply t c) . retract

-- | 'extractR' converts a 'Rewrite' over a 'Generic' into a rewrite over a specific expression type.
extractR :: (Alternative m, Monad m, Term a) => Rewrite c m (Generic a) -> Rewrite c m a
extractR r =  extractT r >>= retractA
  
-- | 'promoteR' promotes a 'Rewrite' into a 'Generic' 'Rewrite'; other types inside Generic cause failure.
--   'try' can be used to convert a failure-by-default promoteR into a 'id-by-default' promotion.
promoteR  :: (Alternative m, Term a) => Rewrite c m a -> Rewrite c m (Generic a)
promoteR = liftA inject . promoteT

-------------------------------------------------------------------------------

-- | a 'Lens' that lets you view a @Generic a@ node as an @a@ node. 
extractL :: (Alternative m, Term a) => Lens c m (Generic a) a
extractL = lens $ \ c -> retractWithA (\ a -> ((c,a), pure . inject))

-- | a 'Lens' that lets you view an @a@ node as a @Generic a@ node. 
promoteL  :: (Alternative m, Term a) => Lens c m a (Generic a)
promoteL = lens $ \ c a -> pure ((c, inject a), retractA)

-------------------------------------------------------------------------------

-- | 'Walker' captures how we walk over the children of a node, using a specific context @c@ and applicative functor @m@.
class (Applicative m, Term a) => Walker c m a where
  -- | 'crushT' applies a 'Generic' Translate to a common, 'Monoid'al result, to all the interesting children of this node.
  crushT :: Monoid b => Translate c m (Generic a) b -> Translate c m a b

  -- | 'allR' applies 'Generic' rewrites to all the interesting children of this node.
  allR :: (Alternative m, Monad m) => Rewrite c m (Generic a) -> Rewrite c m a

  -- | 'chooseL' constructs a 'Lens' pointing at the n-th interesting child of this node.  
  chooseL :: Alternative m => Int -> Lens c m a (Generic a)

-------------------------------------------------------------------------------

-- | 'WalkerR' is a constraint synonym for the common constraints of the 'Rewrite' traversal combinators. 
type WalkerR c m a = (Alternative m, Monad m, Walker c m a, a ~ Generic a)

-- | apply a 'Rewrite' in a top-down manner.
topdownR :: WalkerR c m a => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
topdownR r = r >-> allR (topdownR r)

-- | apply a 'Rewrite' in a bottom-up manner.
bottomupR :: WalkerR c m a => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
bottomupR r = allR (bottomupR r) >-> r

-- | apply a 'Rewrite' in a top-down manner, prunning at successful rewrites.
alltdR :: WalkerR c m a => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
alltdR r = r <+ allR (alltdR r)

-- | apply a 'Rewrite' twice, in a top-down and bottom-up way, using one single tree traversal.
downupR :: WalkerR c m a => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
downupR r = r >-> allR (downupR r) >-> r

-- | a fixed-point traveral, starting with the innermost term.
innermostR :: WalkerR c m a => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
innermostR r = bottomupR (tryR (r >-> innermostR r))

-------------------------------------------------------------------------------

-- | 'WalkerT' is a constraint synonym for the common constraints of the 'Translate' traversal combinators. 
type WalkerT c m a b = (Walker c m a, a ~ Generic a, Monoid b)

-- | fold a tree in a top-down manner, using a single 'Translate' for each node.
topdownT :: WalkerT c m a b => Translate c m (Generic a) b -> Translate c m (Generic a) b
topdownT t = concatT [ t, crushT (topdownT t) ]

-- | fold a tree in a bottom-up manner, using a single 'Translate' for each node.
bottomupT :: WalkerT c m a b => Translate c m (Generic a) b -> Translate c m (Generic a) b
bottomupT t = concatT [ crushT (bottomupT t), t ]

-------------------------------------------------------------------------------

-- | construct a 'Lens' by following a path of 'Int's, where each 'Int' specifies which interesting child to focus on at each step.
pathL :: (Alternative m, Monad m, Walker c m a, a ~ Generic a) => [Int] -> Lens c m (Generic a) (Generic a)
pathL = sequenceL . map chooseL

-------------------------------------------------------------------------------
