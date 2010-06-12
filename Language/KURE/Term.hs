{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-- | This module supports the generic walking of 'Term's. 
--
-- The key idea here is that for each type of expression (@exp@), 
-- we have a sum of all the interesting children types (@Generic exp@).
-- There is always a type that its own 'Generic', which is used for the 
-- deeper syntax tree walks.

module Language.KURE.Term 
	( Term(..)
	) where
	
import Language.KURE.Translate	
import Language.KURE.Rewrite
import Language.KURE.RewriteMonad

import Control.Monad
import Data.Monoid

-- | 'Term's are things that syntax are built from.
class Term exp where
  -- | 'Generic' is a sum of all the interesting sub-types, transitively, of @exp@. 
  -- We use @Generic e ~ e@ to signify that something is its own Generic.
  -- Simple expression types might be their own sole 'Generic', more complex examples
  -- will have a new datatype for the 'Generic', which will also be an instance of class 'Term'.
  type Generic exp

  -- | 'project' projects into a 'Generic', to get the exp inside, or fails.
  select :: Generic exp -> Maybe exp

  -- | 'inject' injects an exp into a 'Generic'.
  inject  :: exp -> Generic exp

  -- | 'equals' creates an predicate 'Translate' specialized to the first argument.
  equals :: exp -> Translate exp Bool
  equals _ = translate $ \ _ -> return False

  -- | 'allR' applies 'Generic' rewrites to all the interesting children of this node.
  allR :: Rewrite (Generic exp) -> Rewrite exp
  -- | 'allU' applied a 'Generic' Translation to a common, 'Monoid'al result, to all the interesting children of this node.
  crushU :: (Monoid result) => Translate (Generic exp) result -> Translate exp result

------------------------------------------------------------------------------
{-

-------------------------------------------------------------------------------
-}
