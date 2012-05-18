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
--
--

module Language.KURE  -- Temperary, needs better organising
	( module Language.KURE.Translate
	, module Language.KURE.Term
        , module Language.KURE.Injection
        , module Language.KURE.Utilities
	) where

import Language.KURE.Injection
import Language.KURE.Translate
import Language.KURE.Term
import Language.KURE.Utilities