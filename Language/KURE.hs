-- |
-- Module: Language.KURE
-- Copyright: (c) 2006-2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: alpha
-- Portability: ghc
--
-- This is the main import module for KURE, which exports all the major components.
-- Note that Injection and Utilities are not exported here.
--   Injection will only be needed once, when definining a Generic data type.
--   Utilities are not required, but may be useful when defining Walker instances.

module Language.KURE
	( module Language.KURE.Translate
	, module Language.KURE.Walker
	) where

import Language.KURE.Translate
import Language.KURE.Walker