{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, KindSignatures, FlexibleInstances #-}
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
        , retractWith
        , retractWithA  
        , retractA
        , extractR
        , promoteR
        , extractT
        , promoteT  
        , extractL  
        , promoteL
        , WalkerR, allR, anyR, allRgeneric, anyRgeneric
        , alltdR
        , anytdR
        , allbuR
        , anybuR
        , allduR
        , anyduR
        , tdpruneR
        , innermostR
        , WalkerT, crushT, crushTgeneric      
        , crushtdT
        , crushbuT
        , tdpruneT                   
        , WalkerL, chooseL, chooseLgeneric 
        , Path  
        , pathL  
        , exhaustPathL  
        , repeatPathL  
) where

import Language.KURE.Translate

import Data.Monoid
import Control.Applicative
import Control.Arrow (second)

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
  
-- | attempts to extract an @a@ from a @Generic a@, and then maps a monadic function over it.
--   can be useful when defining 'chooseL' instances.
retractWith :: (Alternative m, Term a) => (a -> m b) -> Generic a -> m b
retractWith f = maybe empty f . retract

-- | attempts to extract an @a@ from a @Generic a@, and then maps a function over it.
--   can be useful when defining 'chooseL' instances.
retractWithA :: (Alternative m, Term a) => (a -> b) -> Generic a -> m b
retractWithA f = retractWith (pure.f)

-- | attempts to extract an @a@ from a @Generic a@.
retractA :: (Alternative m, Term a) => Generic a -> m a
retractA = retractWithA id

--------------------------------------------------------------------------------

-- | 'extractT' converts a 'Translate' taking a 'Generic' into a translate over a specific expression type.
extractT :: Term a => Translate c m (Generic a) b -> Translate c m a b
extractT t = translate $ \ c -> apply t c . inject

-- | 'promoteT' promotes a 'Translate' into a 'Generic' 'Translate'; other types inside Generic cause failure.
promoteT  :: (Alternative m, Term a) => Translate c m a b -> Translate c m (Generic a) b
promoteT t = translate $ \ c -> retractWith (apply t c)

-- | 'extractR' converts a 'Rewrite' over a 'Generic' into a rewrite over a specific expression type.
extractR :: (Alternative m, Monad m, Term a) => Rewrite c m (Generic a) -> Rewrite c m a
extractR r =  extractT r >>= retractA
  
-- | 'promoteR' promotes a 'Rewrite' into a 'Generic' 'Rewrite'; other types inside 'Generic' cause failure.
--   'tryR' can be used to convert a failure-by-default 'promoteR' into a 'id-by-default' promotion.
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

-- | 'WalkerR' captures the ability to walk over a 'Term' applying rewrites, 
--   using a specific context @c@ and a 'Monad' @m@.
class (Alternative m, Monad m, Term a) => WalkerR c m a where
  
  -- | 'allR' applies 'Generic' rewrites to all the interesting children of this node,
  --   succeeding if they all succeed.
  allR :: Rewrite c m (Generic a) -> Rewrite c m a

  -- | 'anyR' applies 'Generic' rewrites to all the interesting children of this node,
  --   suceeding if any succeed.
  anyR :: Rewrite c m (Generic a) -> Rewrite c m a

-- | 'allRgeneric' is a utility to aid with defining 'WalkerR' instances for the 'Generic' type. 
--   See the "expr" example.  
allRgeneric :: WalkerR c m a => Rewrite c m (Generic a) -> c -> a -> m (Generic a)
allRgeneric r c a = inject <$> apply (allR r) c a

-- | 'anyRgeneric' is a utility to aid with defining 'WalkerR' instances for the 'Generic' type. 
--   See the "expr" example.
anyRgeneric :: WalkerR c m a => Rewrite c m (Generic a) -> c -> a -> m (Generic a)
anyRgeneric r c a = inject <$> apply (anyR r) c a
 
-------------------------------------------------------------------------------

-- | apply a 'Rewrite' in a top-down manner, succeeding if they all succeed.
alltdR :: (WalkerR c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
alltdR r = r >-> allR (alltdR r)

-- | apply a 'Rewrite' in a top-down manner, succeeding if any succeed.
anytdR :: (WalkerR c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
anytdR r = r >+> anyR (anytdR r)

-- | apply a 'Rewrite' in a bottom-up manner, succeeding if they all succeed.
allbuR :: (WalkerR c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
allbuR r = allR (allbuR r) >-> r

-- | apply a 'Rewrite' in a bottom-up manner, succeeding if any succeed.
anybuR :: (WalkerR c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
anybuR r = anyR (anybuR r) >+> r

-- | apply a 'Rewrite' twice, in a top-down and bottom-up way, using one single tree traversal,
--   succeeding if they all succeed.
allduR :: (WalkerR c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
allduR r = r >-> allR (allduR r) >-> r

-- | apply a 'Rewrite' twice, in a top-down and bottom-up way, using one single tree traversal,
--   succeeding if any succeed.
anyduR :: (WalkerR c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
anyduR r = r >+> anyR (anyduR r) >+> r

-- | attempt to apply a 'Rewrite' in a top-down manner, prunning at successful rewrites.
tdpruneR :: (WalkerR c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
tdpruneR r = r <+ anyR (tdpruneR r)

-- | a fixed-point traveral, starting with the innermost term.
innermostR :: (WalkerR c m a, a ~ Generic a) => Rewrite c m (Generic a) -> Rewrite c m (Generic a)
innermostR r = allbuR (tryR (r >-> innermostR r))

-------------------------------------------------------------------------------

-- | 'WalkerT' captures the ability to walk over a 'Term' applying translations, 
--   using a specific context @c@ and an 'Applicative' @m@.
class (Applicative m, Term a, Monoid b) => WalkerT c m a b where
  
  -- | 'crushT' applies a 'Generic' Translate to a common, 'Monoid'al result, to all the interesting children of this node.
  crushT :: Translate c m (Generic a) b -> Translate c m a b

-- | 'crushTgeneric' is a utility to aid with defining 'WalkerT' instances for the 'Generic' type.
--   See the "expr" example.  
crushTgeneric :: WalkerT c m a b => Translate c m (Generic a) b -> c -> a -> m b
crushTgeneric t = apply (crushT t)

-------------------------------------------------------------------------------

-- | fold a tree in a top-down manner, using a single 'Translate' for each node.
crushtdT :: (WalkerT c m a b, a ~ Generic a) => Translate c m (Generic a) b -> Translate c m (Generic a) b
crushtdT t = concatT [ t, crushT (crushtdT t) ]

-- | fold a tree in a bottom-up manner, using a single 'Translate' for each node.
crushbuT :: (WalkerT c m a b, a ~ Generic a) => Translate c m (Generic a) b -> Translate c m (Generic a) b
crushbuT t = concatT [ crushT (crushbuT t), t ]

-- | attempt to apply a 'Translate' in a top-down manner, prunning at successes.
tdpruneT :: (Alternative m, WalkerT c m a b, a ~ Generic a) => Translate c m (Generic a) b -> Translate c m (Generic a) b
tdpruneT t = t <+ crushT (tdpruneT t)

-------------------------------------------------------------------------------

-- | 'WalkerL' captures the ability to construct a 'Lens' into a 'Term', 
--   using a specific context @c@ and an 'Alternative' @m@.
class (Alternative m, Monad m, Term a) => WalkerL c m a where

  -- | 'chooseL' constructs a 'Lens' pointing at the n-th interesting child of this node.  
  chooseL :: Int -> Lens c m a (Generic a)

-- | 'chooseLgeneric' is a utility to aid with defining 'WalkerL' instances for the 'Generic' type. 
--   See the "expr" example.  
chooseLgeneric :: WalkerL c m a => Int -> c -> a -> m ((c, Generic a), Generic a -> m (Generic a))
chooseLgeneric n c a = (second.result.liftA) inject <$> apply (chooseL n) c a
                       
-------------------------------------------------------------------------------

-- | a 'Path' is a list of 'Int's, where each 'Int' specifies which interesting child to descend to at each step.
type Path = [Int]

-- | construct a 'Lens' by following a 'Path'.
pathL :: (WalkerL c m a, a ~ Generic a) => Path -> Lens c m (Generic a) (Generic a)
pathL = sequenceL . map chooseL

-- | construct a 'Lens' that points to the last node at which the 'Path' can be followed.
exhaustPathL :: (WalkerL c m a, a ~ Generic a) => Path -> Lens c m (Generic a) (Generic a)
exhaustPathL []     = idL 
exhaustPathL (n:ns) = tryL (chooseL n `composeL` exhaustPathL ns)

-- | repeat as many iterations of the 'Path' as possible.
repeatPathL :: (WalkerL c m a, a ~ Generic a) => Path -> Lens c m (Generic a) (Generic a)
repeatPathL p = tryL (pathL p `composeL` repeatPathL p)

-------------------------------------------------------------------------------

-- Utilities

-- One of Conal Elliott's semantic editor combinators.
result :: (b -> c) -> (a -> b) -> (a -> c)
result = (.)

-------------------------------------------------------------------------------
