-- |
-- Module: Language.KURE
-- Copyright: (c) 2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- This is the main import module for KURE, which exports all the major components.
--
--

module Language.KURE 
	( module Language.KURE.Translate
	, module Language.KURE.Term
--	, module Language.KURE.Boilerplate
	) where

import Language.KURE.Translate
import Language.KURE.Term
-- import Language.KURE.Boilerplate