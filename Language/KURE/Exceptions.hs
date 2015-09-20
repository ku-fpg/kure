{-# LANGUAGE InstanceSigs #-}

-- |
-- Module: Language.KURE.MonadCatch
-- Copyright: (c) 2012--2015 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides KURE-specific 'Exception' idioms.
module Language.KURE.Exceptions
        ( -- ** Exceptions
          showKureExc
        , NodeMismatch(..)
        , nodeMismatch
        , displayNodeMismatch
        , StrategyFailure(..)
        , strategyFailure
        , stackStrategyFailure
        , displayStrategyFailure
        , ConditionalFailure(..)
        , conditionalFailure
        , displayConditionalFailure
        ) where

import Control.Exception (Exception(..), SomeException(..))

-- | A node mismatch exception type, containing the name of the node that
--   was mismatched.
newtype NodeMismatch = NodeMismatch String
  deriving (Eq, Show)

instance Exception NodeMismatch where
  displayException :: NodeMismatch -> String
  displayException = displayNodeMismatch
  {-# INLINE displayException #-}

-- | Construct a 'NodeMismatch' from a node name.
nodeMismatch :: String -> NodeMismatch
nodeMismatch = NodeMismatch
{-# INLINE nodeMismatch #-}

-- | Show a 'NodeMismatch' in a human-friendly way.
displayNodeMismatch :: NodeMismatch -> String
displayNodeMismatch (NodeMismatch n) = "the node was not a " ++ n ++ "."
{-# INLINE displayNodeMismatch #-}

-- | A strategy failure exception type, containing the name of the failed
--   strategy and, if possible, the reason why it failed.
data StrategyFailure = StrategyFailure String (Maybe SomeException)
  deriving Show

instance Exception StrategyFailure where
  displayException :: StrategyFailure -> String
  displayException = displayStrategyFailure
  {-# INLINE displayException #-}

-- | Construct a 'StrategyFailure' from a strategy name with no explanation
--   for why it failed.
strategyFailure :: String -> StrategyFailure
strategyFailure s = StrategyFailure s Nothing
{-# INLINE strategyFailure #-}

-- | Construct a 'StrategyFailure' from a strategy name and an explanation
--   for why it failed.
stackStrategyFailure :: String -> SomeException -> StrategyFailure
stackStrategyFailure s = StrategyFailure s . Just
{-# INLINE stackStrategyFailure #-}

-- | Show a 'StrategyFailure' in a human-friendly way.
displayStrategyFailure :: StrategyFailure -> String
displayStrategyFailure (StrategyFailure s Nothing) =
    "the " ++ s ++ " strategy failed."
displayStrategyFailure (StrategyFailure s (Just e)) =
    "the " ++ s ++ " strategy failed, because " ++ showKureExc e

-- | A conditional test (other than a 'NodeMismatch') failure exception type,
--   containing an explanation of what failed and why.
data ConditionalFailure = ConditionalFailure String
  deriving (Eq, Show)

instance Exception ConditionalFailure where
  displayException :: ConditionalFailure -> String
  displayException = displayConditionalFailure
  {-# INLINE displayException #-}

-- | Construct a 'ConditionalFailure' from an explanation as for what failed and why.
conditionalFailure :: String -> ConditionalFailure
conditionalFailure = ConditionalFailure
{-# INLINE conditionalFailure #-}

-- | Show a 'ConditionalFailure' in a human-friendly way.
displayConditionalFailure :: ConditionalFailure -> String
displayConditionalFailure (ConditionalFailure why) = why
{-# INLINE displayConditionalFailure #-}

-- | If 'SomeException' contains a KURE-related 'Exception', show it in
--   a human-friendly way.
showKureExc :: SomeException -> String
showKureExc = displayException
