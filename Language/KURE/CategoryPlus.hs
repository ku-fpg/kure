{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Language.KURE.CategoryPlus
-- Copyright: (c) 2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides classes that form a monoid on 'Category's.
-- This is analagous to 'Control.Arrow.ArrowPlus', but without requiring the 'Category' be an 'Control.Arrow.Arrow'.

module Language.KURE.CategoryPlus
           ( -- * A Monoid on Categories
             CategoryZero(..)
           , CategoryPlus(..)
) where

import Control.Category

infixl 3 <+

----------------------------------------------------------------------

-- | Categories with a zero (fail).
class Category (~>) => CategoryZero (~>) where
  -- | The zero arrow.
  czero :: a ~> b

-- | A monoid on categories.
class CategoryZero (~>) => CategoryPlus (~>) where
  -- | An associative operation with identity 'czero'.
  (<+) :: (a ~> b) -> (a ~> b) -> (a ~> b)

----------------------------------------------------------------------
